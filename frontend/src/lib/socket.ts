import { io } from 'socket.io-client'
import SimplePeer from 'simple-peer'
import { textDecoder } from '../crypto'
import { IV_LENGTH } from '../const'
import { key } from '../stores/credentials'
import { is_client } from 'svelte/internal'

const socket = io('http://localhost:4000')

export const emitUsername = (username: string) => {
	socket.emit('username', username)
}

const peers = new Map<string, SimplePeer.Instance>()

export const sendToAllPeers = (data: SimplePeer.SimplePeerData) => {
	;[...peers].forEach(([, peer]) => peer.send(data))
}

const addPeer = (
	peer: SimplePeer.Instance,
	socketId: string,
): SimplePeer.Instance => {
	peer.on('connect', () => {
		console.log(`CONNECTED to ${socketId}`)
	})
	key.subscribe((keyValue) => {
		peer.removeAllListeners('data')
		peer.on('data', async (data: Uint8Array) => {
			console.log(data)
			const iv = data.subarray(0, IV_LENGTH)
			const encryptedTextArray = data.subarray(IV_LENGTH)
			const decrypted = await crypto.subtle.decrypt(
				{
					name: 'AES-GCM',
					iv,
				},
				await keyValue,
				encryptedTextArray,
			)
			const { data: d } = JSON.parse(textDecoder.decode(decrypted))
			console.log(d)
		})
	})
	peer.on('error', (e) => {
		const { error: { code } = {} } = (e as unknown) as {
			error?: { code?: number }
		}
		if (code === 0) {
			peers.get(socketId).destroy()
			peers.delete(socketId)
		}
	})
	peer.on('signal', (signal) => {
		console.log(signal)
		socket.emit('signal', {
			socketIdTo: socketId,
			signal,
		})
	})

	peers.set(socketId, peer)

	return peer
}

if (is_client) {
	socket.on('socketIds', (socketIds: Array<string>) => {
		socketIds.forEach((socketId) =>
			addPeer(
				new SimplePeer({
					initiator: true,
					trickle: false,
				}),
				socketId,
			),
		)
	})

	socket.on('signal', ({ socketIdFrom, signal }) => {
		if (signal.type === 'offer') {
			const peer = addPeer(
				new SimplePeer({
					trickle: false,
				}),
				socketIdFrom,
			)

			peer.signal(signal)
		} else if (signal.type === 'answer') {
			peers.get(socketIdFrom).signal(signal)
		}
	})
}
