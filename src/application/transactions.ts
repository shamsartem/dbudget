import type { JSONSchemaType } from 'ajv'
import { serialize, deserialize } from 'bson'

import { ajv } from './ajv'
import { sendToElm } from './elm'
import { store } from './store'

type Transactions = Array<Array<string>>

export const transactionsSchema: JSONSchemaType<Transactions> = {
  type: 'array',
  items: { type: 'array', items: { type: 'string' } },
}

export const validateTransactions = ajv.compile(transactionsSchema)

const IV_LENGTH = 12
const SALT_LENGTH = 16
const IV_AND_SALT_LENGTH = IV_LENGTH + SALT_LENGTH

const ALGORITHM = 'AES-GCM'

const textEncoder = new TextEncoder()

const getEncryptionKey = async (
  password: string,
  salt: Uint8Array,
  encrypt = false,
): Promise<CryptoKey> => {
  const passwordKey = await crypto.subtle.importKey(
    'raw',
    textEncoder.encode(password),
    'PBKDF2',
    false,
    ['deriveKey'],
  )

  return crypto.subtle.deriveKey(
    {
      name: 'PBKDF2',
      salt,
      iterations: 250000,
      hash: 'SHA-256',
    },
    passwordKey,
    { name: 'AES-GCM', length: 256 },
    false,
    [encrypt ? 'encrypt' : 'decrypt'],
  )
}

export const encrypt = async (
  transactions: Array<Array<string>>,
): Promise<Uint8Array | undefined> => {
  if (store.cred === null) {
    sendToElm('Toast', "Can't encrypt. You signed out")
    return
  }
  const salt = crypto.getRandomValues(new Uint8Array(SALT_LENGTH))
  const iv = crypto.getRandomValues(new Uint8Array(IV_LENGTH))
  const encrypted: unknown = await crypto.subtle.encrypt(
    {
      name: ALGORITHM,
      iv,
    },
    await getEncryptionKey(store.cred.password, salt, true),
    serialize({ transactions }),
  )

  if (!(encrypted instanceof ArrayBuffer)) {
    sendToElm('Toast', 'Incorrect data after encryption')
    return
  }

  const encryptedUint8Array = new Uint8Array(encrypted)
  const uint8ArrayWithSalt = new Uint8Array(
    IV_AND_SALT_LENGTH + encryptedUint8Array.byteLength,
  )
  uint8ArrayWithSalt.set(salt, 0)
  uint8ArrayWithSalt.set(iv, SALT_LENGTH)
  uint8ArrayWithSalt.set(encryptedUint8Array, IV_AND_SALT_LENGTH)
  return uint8ArrayWithSalt
}

export const decrypt = async ({
  arrayBuffer,
  password,
}: {
  arrayBuffer: Uint8Array
  password: string
}): Promise<Transactions | undefined> => {
  const salt = arrayBuffer.slice(0, SALT_LENGTH)
  const iv = arrayBuffer.slice(SALT_LENGTH, IV_AND_SALT_LENGTH)
  const encryptedArray = arrayBuffer.slice(IV_AND_SALT_LENGTH)
  const decrypted: unknown = await crypto.subtle.decrypt(
    {
      name: ALGORITHM,
      iv,
    },
    await getEncryptionKey(password, salt),
    encryptedArray,
  )

  if (!(decrypted instanceof ArrayBuffer)) {
    sendToElm('Toast', 'Decrypted data has wrong format')
    return
  }

  const { transactions } = deserialize(decrypted)

  if (!validateTransactions(transactions)) {
    sendToElm('Toast', 'Deserialized data has wrong format')
    return
  }

  return transactions
}
