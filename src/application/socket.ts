import { Buffer } from 'buffer'

import { io, Socket } from 'socket.io-client'
import type { JSONSchemaType } from 'ajv'

import { sendToElm } from './elm'
import { store } from './store'
import { Transactions, transactionsSchema } from './transactions'
import { ajv } from './ajv'
import { compressString, decompress, decrypt, encrypt } from './crypto'

type SimplePeerInstance = InstanceType<typeof window.SimplePeer>

type PeerMessage =
  | {
      msg: 'hello'
      payload: {
        deviceName: string
        dataToDecrypt: string
      }
    }
  | {
      msg: 'helloBack'
      payload: {
        decryptedData: string
      }
    }
  | {
      msg: 'transactions'
      payload: Transactions
    }
  | {
      msg: 'finishedSendingTransactions'
    }
  | {
      msg: 'backedUpTransactions'
    }
  | {
      msg: 'transactionConfirmation'
    }

const peerMessageSchema: JSONSchemaType<PeerMessage> = {
  type: 'object',
  oneOf: [
    {
      properties: {
        msg: { type: 'string', enum: ['hello'] },
        payload: {
          type: 'object',
          properties: {
            deviceName: { type: 'string' },
            dataToDecrypt: { type: 'string' },
          },
        },
      },
    },
    {
      properties: {
        msg: { type: 'string', enum: ['helloBack'] },
        payload: {
          type: 'object',
          properties: {
            latestTransactionTimestamp: { type: 'string' },
            decryptedData: { type: 'string' },
          },
        },
      },
    },
    {
      properties: {
        msg: { type: 'string', enum: ['transactions'] },
        payload: transactionsSchema,
      },
    },
    {
      properties: {
        msg: { type: 'string', enum: ['finishedSendingTransactions'] },
      },
    },
    {
      properties: {
        msg: { type: 'string', enum: ['backedUpTransactions'] },
      },
    },
    {
      properties: {
        msg: { type: 'string', enum: ['transactionConfirmation'] },
      },
    },
  ],
  required: ['msg'],
}

const validatePeerMessage = ajv.compile(peerMessageSchema)

class Peer {
  dataToEncrypt: string
  socketId: string
  deviceName?: string
  #isTrusted = false
  #p: SimplePeerInstance
  #sentTransactionChunks = 0
  #receivedConfirmations = 0
  constructor({
    initiator,
    socketId,
  }: {
    initiator: boolean
    socketId: string
  }) {
    this.socketId = socketId
    this.dataToEncrypt = window.crypto.randomUUID()
    this.#p = new window.SimplePeer({
      initiator,
      trickle: false,
      objectMode: true,
    })
    this.#p.on('error', (): void => {
      this.cleanUpAndDestroy()
    })
    this.#p.on('close', (): void => {
      this.cleanUpAndDestroy()
    })
    this.#p.on('signal', (signalData): void => {
      if (store.cred === null) {
        this.cleanUpAndDestroy()
        return
      }

      socket.send({
        msg: 'signal',
        payload: {
          signalData: JSON.stringify(signalData),
          socketId,
          username: store.cred.username,
        },
      })
    })
    this.#p.on('data', (data): void => {
      if (typeof data !== 'string') {
        this.cleanUpAndDestroy()
        return
      }
      decompress(data)
        .then((data): void => {
          if (store.cred === null || typeof data !== 'string') {
            this.cleanUpAndDestroy()
            return
          }
          const parsedData: unknown = JSON.parse(data)
          if (!validatePeerMessage(parsedData)) {
            sendToElm(
              'Toast',
              `Peer sent invalid data: ${JSON.stringify(parsedData)}`,
            )
            return
          }

          switch (parsedData.msg) {
            case 'hello': {
              const { dataToDecrypt } = parsedData.payload
              this.deviceName = parsedData.payload.deviceName
              decrypt(
                new Uint8Array(Buffer.from(dataToDecrypt, 'base64')),
                store.cred.password,
              )
                .then((decryptedData): void => {
                  if (typeof decryptedData !== 'string') {
                    throw new Error(
                      `Decrypted is not string: ${String(decryptedData)}`,
                    )
                  }
                  this.write({
                    msg: 'helloBack',
                    payload: {
                      decryptedData,
                    },
                  })
                })
                .catch((): void => {
                  sendToElm(
                    'Toast',
                    'Make sure you use the same password with all of your devices',
                  )
                })
              break
            }
            case 'helloBack': {
              if (parsedData.payload.decryptedData !== this.dataToEncrypt) {
                sendToElm('Toast', 'Somebody is hacking you!')
                this.cleanUpAndDestroy()
                return
              }
              this.#isTrusted = true
              sendToElm('GotHelloBack', this.socketId)
              break
            }

            case 'transactions': {
              sendToElm('ReceivedTransactions', {
                transactions: parsedData.payload,
                socketId: this.socketId,
              })
              break
            }

            case 'transactionConfirmation': {
              this.#receivedConfirmations++
              if (
                this.#sentTransactionChunks > 0 &&
                this.#receivedConfirmations >= this.#sentTransactionChunks
              ) {
                this.write({
                  msg: 'finishedSendingTransactions',
                })
                this.#sentTransactionChunks = 0
                this.#receivedConfirmations = 0
              }
              break
            }

            case 'finishedSendingTransactions': {
              sendToElm(
                'SyncComplete',
                `Got transactions from ${this.deviceName ?? ''}`,
              )
              this.write({
                msg: 'backedUpTransactions',
              })
              break
            }

            case 'backedUpTransactions': {
              sendToElm(
                'Toast',
                `Sent transactions to ${this.deviceName ?? ''}`,
              )
              break
            }

            default: {
              const _exhaustiveCheck: never = parsedData
              return _exhaustiveCheck
            }
          }
        })
        .catch((): void => {
          sendToElm('Toast', 'Unable to decompress data')
        })
    })
    this.#hello().catch((e): void => {
      sendToElm('Toast', `Can't send hello: ${String(e)}`)
    })
  }
  cleanUpAndDestroy(): void {
    this.destroy()
    peersBySocketId.delete(this.socketId)
  }
  async #hello(): Promise<void> {
    if (store.cred === null) {
      this.cleanUpAndDestroy()
      return
    }
    this.write({
      msg: 'hello',
      payload: {
        dataToDecrypt: Buffer.from(
          await encrypt(this.dataToEncrypt, store.cred.password),
        ).toString('base64'),
        deviceName: store.cred.deviceName,
      },
    })
  }
  signal(...args: Parameters<SimplePeerInstance['signal']>): void {
    this.#p.signal(...args)
  }
  destroy(...args: Parameters<SimplePeerInstance['destroy']>): void {
    this.#p.destroy(...args)
  }
  write(message: PeerMessage): void {
    if (!['hello', 'helloBack'].includes(message.msg) && !this.#isTrusted) {
      return
    }
    compressString(JSON.stringify(message))
      .then((compressedMessage): void => {
        this.#p.write(compressedMessage)
      })
      .catch((e): void => {
        console.error(`Not able to compress message: ${String(e)}`)
      })
  }
  sendTransactions(transactions: Transactions): void {
    const CHUNK_SIZE = 1000
    const chunks = Math.ceil(transactions.length / CHUNK_SIZE)
    this.#sentTransactionChunks = chunks
    this.#receivedConfirmations = 0

    for (let i = 0; i < transactions.length; i += CHUNK_SIZE) {
      this.write({
        msg: 'transactions',
        payload: transactions.slice(i, i + CHUNK_SIZE),
      })
    }

    // If no chunks were sent (empty transactions), send finished immediately
    if (chunks === 0) {
      this.write({
        msg: 'finishedSendingTransactions',
      })
      this.#sentTransactionChunks = 0
      this.#receivedConfirmations = 0
    }
  }
}

const peersBySocketId = new Map<string, Peer>()

export const cleanupPeers = (): void => {
  peersBySocketId.forEach((peer): void => {
    peer.destroy()
  })
  peersBySocketId.clear()
}

export const sendTransactionsToAll = (transactions: Transactions): void => {
  if (peersBySocketId.size === 0) {
    return
  }

  peersBySocketId.forEach((peer): void => {
    sendTransactionsToPeer(peer.socketId, transactions)
  })
}

export const sendTransactionsToPeer = (
  socketId: string,
  transactions: Transactions,
): void => {
  const peer = peersBySocketId.get(socketId)
  if (peer === undefined) {
    return
  }
  peer.sendTransactions(transactions)
}

export const sendMessageToPeer = (
  socketId: string,
  message: PeerMessage,
): void => peersBySocketId.get(socketId)?.write(message)

const onSocketIdsSchema: JSONSchemaType<{
  socketIds: Array<string>
}> = {
  type: 'object',
  properties: {
    socketIds: {
      type: 'array',
      items: { type: 'string' },
    },
  },
  required: ['socketIds'],
}
const validateOnSocketIds = ajv.compile(onSocketIdsSchema)

const onSignalSchema: JSONSchemaType<{
  socketId: string
  signalData: string
}> = {
  type: 'object',
  properties: {
    socketId: { type: 'string' },
    signalData: { type: 'string' },
  },
  required: ['socketId', 'signalData'],
}
const validateOnSignalSchema = ajv.compile(onSignalSchema)

type SocketSendMessage =
  | {
      msg: 'init'
      payload: string
    }
  | {
      msg: 'signal'
      payload: {
        socketId: string
        signalData: string
        username: string
      }
    }
class SocketWrapper {
  #socket?: Socket | undefined
  connect(server: string): void {
    this.#socket = io(server)

    this.#socket.on('connect', (): void => {
      if (
        store.cred === null ||
        this.#socket === undefined ||
        this.#socket.disconnected
      ) {
        return
      }
      sendToElm('Toast', 'Connected to signaling server')
      this.send({ msg: 'init', payload: store.cred.username })
    })

    this.#socket.on('socketIds', (payload: unknown): void => {
      if (!validateOnSocketIds(payload)) {
        sendToElm('Toast', 'Server sent invalid data')
        return
      }

      const filteredSocketIds = payload.socketIds.filter(
        (socketId): boolean =>
          this.#socket !== undefined && socketId !== this.#socket.id,
      )
      for (const socketId of filteredSocketIds) {
        peersBySocketId.set(socketId, new Peer({ initiator: true, socketId }))
      }
    })

    this.#socket.on('signal', (payload: unknown): void => {
      if (!validateOnSignalSchema(payload)) {
        console.error(
          'signal is not an object with socketId and signalData: unrecoverable error',
        )
        return
      }
      const { socketId, signalData } = payload
      let peer = peersBySocketId.get(socketId)
      if (peer === undefined) {
        peer = new Peer({ initiator: false, socketId })
        peersBySocketId.set(socketId, peer)
      }
      peer.signal(signalData)
    })
  }
  send({ msg, payload }: SocketSendMessage): void {
    if (this.#socket === undefined) {
      throw new Error('`send` method called before `connect`')
    }
    this.#socket.send({
      app: 'dbudget',
      data: { msg, ...(payload === undefined ? {} : { payload }) },
    })
  }
  disconnect(): void {
    this.#socket?.disconnect()
    this.#socket = undefined
  }
}

export const socket = new SocketWrapper()
