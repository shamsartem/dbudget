const express = require('express')
const app = express()
const http = require('http').createServer(app)
const io = require('socket.io')(http, {
	cors: {
		origin: 'http://localhost:3000',
	},
})

app.use(express.static('../frontend/__sapper__/export'))

const usernameSocketIds = new Map()
const socketIdUsername = new Map()

io.on('connection', (socket) => {
	socket.on('username', (username) => {
		console.log(username)
		if (usernameSocketIds.has(username)) {
			const socketIds = usernameSocketIds.get(username)
			socket.emit('socketIds', [...socketIds])
			socketIds.add(socket.id)
			usernameSocketIds.set(username, socketIds)
		} else {
			usernameSocketIds.set(username, new Set([socket.id]))
		}

		socketIdUsername.set(socket.id, username)
	})

	socket.on('signal', ({ socketIdTo, signal }) => {
		console.log({ socketIdTo, signal })
		socket.to(socketIdTo).emit('signal', { socketIdFrom: socket.id, signal })
	})

	socket.on('disconnect', () => {
		const username = socketIdUsername.get(socket.id)
		if (!username) {
			return
		}
		socketIdUsername.delete(socket.id)
		const room = usernameSocketIds.get(username)
		room.delete(socket.id)
		usernameSocketIds.set(username, room)
	})
})

http.listen(4000, () => {
	console.log('listening on *:4000')
})
