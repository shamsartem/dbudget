import { io } from 'socket.io-client'

import { hasKey, isUint8Array } from './typeHelpers'
import { getEncrypted } from './idb'
import { sendToElm } from './elm'
import { store } from './store'
import { decrypt } from './transactions'

type Peer = InstanceType<typeof window.SimplePeer>

const peers = new Map<string, Peer>()

export const cleanupPeers = (): void => {
  peers.forEach((peer): void => {
    peer.destroy()
  })
  peers.clear()
}

export const sendToAll = async (): Promise<void> => {
  if (store.cred === null) {
    return
  }

  const encrypted = await getEncrypted(store.cred.username)
  if (encrypted === null) {
    return
  }
  peers.forEach((peer): void => {
    peer.send(encrypted)
  })

  if (peers.size === 1) {
    sendToElm('Toast', 'Sent data to another peer')
  }

  if (peers.size > 1) {
    sendToElm('Toast', `Sent data to ${peers.size} peers`)
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
        p.send(encrypted)
        sendToElm('Toast', 'Sent data to another peer')
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

  p.on('error', (): void => {
    p.destroy()
    peers.delete(socketId)
  })

  p.on('data', (arrayBuffer: unknown): void => {
    sendToElm('Toast', 'Got data from another peer')
    if (store.cred === null) {
      p.destroy()
      return
    }

    if (!isUint8Array(arrayBuffer)) {
      sendToElm('Toast', 'Got wrong data from remote')
      return
    }

    decrypt({ arrayBuffer, password: store.cred.password })
      .then((transactions): void => {
        if (transactions === undefined) {
          return
        }
        sendToElm('GotTransactions', transactions)
      })
      .catch((): void => {
        sendToElm(
          'Toast',
          'Got data encrypted with a different password. Make sure your devices use the same password',
        )
      })
  })

  p.on('close', (): void => {
    p.destroy()
    peers.delete(socketId)
  })
}

export const socket = io(
  'https://webrtc-mesh-signaling.herokuapp.com',
  // 'http://localhost:3000',
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
