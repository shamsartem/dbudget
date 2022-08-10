import { Buffer } from 'buffer'

import { io } from 'socket.io-client'
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

      sendSocket({
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
              sendToElm('ReceivedTransactions', parsedData.payload)
              break
            }

            case 'finishedSendingTransactions': {
              sendToElm(
                'Toast',
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
                `${this.deviceName ?? ''} backed up transactions from us`,
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

const CHUNK_SIZE = 1000
export const sendTransactionsToPeer = (
  socketId: string,
  transactions: Transactions,
): void => {
  for (let i = 0; i < transactions.length; i += CHUNK_SIZE) {
    peersBySocketId.get(socketId)?.write({
      msg: 'transactions',
      payload: transactions.slice(i, i + CHUNK_SIZE),
    })
  }
  sendMessageToPeer(socketId, {
    msg: 'finishedSendingTransactions',
  })
}

export const sendMessageToPeer = (
  socketId: string,
  message: PeerMessage,
): void => peersBySocketId.get(socketId)?.write(message)

export const socket = io(
  'https://webrtc-mesh-signaling.herokuapp.com',
  // 'http://localhost:4000',
  {
    autoConnect: false,
  },
)

socket.on('connect', (): void => {
  if (store.cred === null || socket.disconnected) {
    return
  }
  sendToElm('Toast', 'Connected to signaling server')
  sendSocket({ msg: 'init', payload: store.cred.username })
})

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
socket.on('socketIds', (payload: unknown): void => {
  if (!validateOnSocketIds(payload)) {
    sendToElm('Toast', 'Server sent invalid data')
    return
  }
  for (const socketId of payload.socketIds.filter(
    (socketId): boolean => socketId !== socket.id,
  )) {
    peersBySocketId.set(socketId, new Peer({ initiator: true, socketId }))
  }
})

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
socket.on('signal', (payload: unknown): void => {
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

const sendSocket = ({ msg, payload }: SocketSendMessage): void => {
  socket.send({
    app: 'dbudget',
    data: { msg, ...(payload === undefined ? {} : { payload }) },
  })
}
