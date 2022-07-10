import { io } from 'socket.io-client'
import { deserialize, serialize } from 'bson'

import { hasKey, hasKeys, isUint8Array } from './typeHelpers'
import { getEncrypted } from './idb'
import { sendToElm } from './elm'
import { store } from './store'
import { decrypt } from './transactions'

type Peer = InstanceType<typeof window.SimplePeer>

const peers = new Map<string, Peer>()
const deviceNames = new Map<string, string>()

export const cleanupPeers = (): void => {
  peers.forEach((peer): void => {
    peer.destroy()
  })
  peers.clear()
  deviceNames.clear()
}

const CHUNK_SIZE = 131_072

type Chunk = {
  subarray: {
    buffer: Uint8Array
  }
  dataLength: number
  index: number
  id: string
  deviceName: string
}

const isChunkValid = (unknown: unknown): unknown is Chunk =>
  hasKeys(unknown, 'subarray', 'index', 'dataLength', 'id', 'deviceName') &&
  hasKey(unknown.subarray, 'buffer') &&
  isUint8Array(unknown.subarray.buffer) &&
  typeof unknown.index === 'number' &&
  typeof unknown.dataLength === 'number' &&
  typeof unknown.id === 'string' &&
  typeof unknown.deviceName === 'string'

const sendChunky = (data: Uint8Array, p: Peer, deviceName: string): void => {
  const id = window.crypto.getRandomValues(new Uint32Array(4)).join('')
  for (let index = 0; index < data.length; index += CHUNK_SIZE) {
    const chunk: Omit<Chunk, 'subarray'> & { subarray: Uint8Array } = {
      subarray: data.slice(index, index + CHUNK_SIZE),
      dataLength: data.length,
      index,
      id,
      deviceName,
    }
    p.send(serialize(chunk))
  }
}

export const sendToAll = (data: Uint8Array, deviceName: string): void => {
  peers.forEach((p): void => {
    sendChunky(data, p, deviceName)
  })

  if (peers.size === 1) {
    sendToElm(
      'Toast',
      `Sent data to ${Array.from(peers.keys())
        .map((socketId): string => deviceNames.get(socketId) ?? socketId)
        .join(', ')}`,
    )
  }
}

const addListeners = (p: Peer, socketId: string): void => {
  p.on('connect', (): void => {
    if (store.cred === null) {
      p.destroy()
      return
    }

    getEncrypted(store.cred.username)
      .then((encrypted): void => {
        if (encrypted === null) {
          return
        }
        if (store.cred === null) {
          p.destroy()
          return
        }
        sendChunky(encrypted, p, store.cred.deviceName)
        sendToElm(
          'Toast',
          `Sent data to ${deviceNames.get(socketId) ?? socketId}`,
        )
      })
      .catch((e): void => {
        console.error(e)
      })
  })

  p.on('signal', (signalData): void => {
    if (store.cred === null) {
      p.destroy()
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

  p.on('error', (e): void => {
    sendToElm('Toast', `Peer communication error ${String(e)}`)
    p.destroy()
    peers.delete(socketId)
    deviceNames.delete(socketId)
  })

  let sendChunkyId: null | string = null
  let data: Array<Uint8Array> = []
  let currentDataLength = 0
  let wholeDataLength = 0

  const cleanUp = (): void => {
    data = []
    currentDataLength = 0
    wholeDataLength = 0
  }

  const cleanUpAndDestroy = (message: string): void => {
    sendToElm('Toast', message)
    p.destroy()
    cleanUp()
  }

  p.on('data', (arrayBuffer: unknown): void => {
    if (store.cred === null) {
      cleanUpAndDestroy("Can't process data. You are signed out")
      return
    }

    if (!(arrayBuffer instanceof Uint8Array)) {
      cleanUpAndDestroy('Got wrong data from remote. Expected: Uint8Array')
      return
    }

    const deserializedData = deserialize(arrayBuffer)
    if (!isChunkValid(deserializedData)) {
      cleanUpAndDestroy('Got wrong data from remote. Expected: Chunk')
      return
    }

    const {
      dataLength,
      index,
      subarray: { buffer: subarray },
      deviceName,
      id,
    } = deserializedData

    deviceNames.set(socketId, deviceName)

    if (sendChunkyId !== id || wholeDataLength !== dataLength) {
      cleanUp()
      sendChunkyId = id
      wholeDataLength = dataLength
    }

    data[index] = subarray
    currentDataLength += subarray.length

    if (currentDataLength === wholeDataLength) {
      sendToElm('Toast', `Got data from ${deviceName}`)
      const arrayBuffer = new Uint8Array(
        data.flatMap((arr): Array<number> => [...arr]),
      )
      decrypt({ arrayBuffer, password: store.cred.password })
        .then((transactions): void => {
          sendToElm('GotTransactions', transactions)
          cleanUp()
        })
        .catch((): void => {
          cleanUpAndDestroy(
            'Got data encrypted with a different password. Make sure your devices use the same password',
          )
        })
    }
  })

  p.on('close', (): void => {
    p.destroy()
    peers.delete(socketId)
    deviceNames.delete(socketId)
  })
}

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

socket.on('socketIds', (payload: unknown): void => {
  if (!hasKey(payload, 'socketIds')) {
    console.error('socketIds payload is missing required key socketIds')
    return
  }
  const { socketIds } = payload
  if (
    !Array.isArray(socketIds) ||
    !socketIds.every((x): x is string => typeof x === 'string')
  ) {
    console.error('socketIds is not an array of strings: unrecoverable error')
    return
  }

  for (const socketId of socketIds.filter(
    (socketId): boolean => socketId !== socket.id,
  )) {
    const p = new window.SimplePeer({
      initiator: true,
      trickle: false,
    })

    addListeners(p, socketId)

    peers.set(socketId, p)
  }
})

socket.on('signal', (data: unknown): void => {
  if (!hasKey(data, 'socketId') || !hasKey(data, 'signalData')) {
    console.error(
      'signal is not an object with socketId and signalData: unrecoverable error',
    )
    return
  }

  const { socketId, signalData } = data

  if (typeof socketId !== 'string' || typeof signalData !== 'string') {
    console.error(
      'signal is not an object with socketId and signalData: unrecoverable error',
    )
    return
  }

  let p = peers.get(socketId)

  if (p === undefined) {
    p = new window.SimplePeer({
      initiator: false,
      trickle: false,
    })

    addListeners(p, socketId)

    peers.set(socketId, p)
  }
  // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
  p.signal(JSON.parse(signalData))
})

type SendOptions =
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

export const sendSocket = ({ msg, payload }: SendOptions): void => {
  socket.send({
    app: 'dbudget',
    data: { msg, ...(payload === undefined ? {} : { payload }) },
  })
}
