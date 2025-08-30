import { WebSocketServer } from 'ws'
import { NodeWSServerAdapter } from '@automerge/automerge-repo-network-websocket'

const port = 8080
const wss = new WebSocketServer({ port })
new NodeWSServerAdapter(wss)
console.log(`Listening on port ${port}...`)
