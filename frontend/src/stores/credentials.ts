import { is_client } from 'svelte/internal'
import { derived, Readable, writable } from 'svelte/store'
import { textEncoder } from '../crypto'
import { getTransactionFromDB } from '../idb'
import { emitUsername } from '../lib/socket'

export const credentials = writable<{
	username: string | null
	passwordHash: Uint8Array | null
}>(
	// {
	// 	username: null,
	// 	passwordHash: null,
	// },
	{
		username: 'sus',
		passwordHash: textEncoder.encode('pppppppppppppppp'),
	},
)

export const key: Readable<Promise<CryptoKey> | Promise<null>> = derived(
	credentials,
	async ({ passwordHash, username }) => {
		if (!is_client || passwordHash === null || username === null) {
			return Promise.resolve(null)
		}
		emitUsername(username)
		getTransactionFromDB().then((transactions) => {
			console.log(transactions)
		})
		return crypto.subtle.importKey(
			'raw',
			passwordHash,
			{
				name: 'AES-GCM',
				length: 128,
			},
			false,
			['encrypt', 'decrypt'],
		)
	},
)
