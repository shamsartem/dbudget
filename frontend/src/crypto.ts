import { is_client } from 'svelte/internal'
import { IV_LENGTH } from './const'

export const iv: Uint8Array | false =
	is_client && crypto.getRandomValues(new Uint8Array(IV_LENGTH))

export const textEncoder = new TextEncoder()

export const textDecoder = new TextDecoder()
