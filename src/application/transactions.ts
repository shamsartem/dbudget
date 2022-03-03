import { serialize, deserialize } from 'bson'

const IV_LENGTH = 12
const SALT_LENGTH = 16
const IV_AND_SALT_LENGTH = IV_LENGTH + SALT_LENGTH

const ALGORITHM = 'AES-GCM'

const textEncoder = new TextEncoder()

const getEncryptionKey = async (
  password: string,
  salt: Uint8Array,
  encrypt = false,
) => {
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
  password: string,
  transactions: string,
): Promise<Uint8Array> => {
  const salt = crypto.getRandomValues(new Uint8Array(SALT_LENGTH))
  const iv = crypto.getRandomValues(new Uint8Array(IV_LENGTH))
  const encrypted = await crypto.subtle.encrypt(
    {
      name: ALGORITHM,
      iv,
    },
    await getEncryptionKey(password, salt, true),
    serialize({ transactions }),
  )
  const encryptedArray = new Uint8Array(encrypted)
  const arrayBuffer = new Uint8Array(
    IV_AND_SALT_LENGTH + encryptedArray.byteLength,
  )
  arrayBuffer.set(salt, 0)
  arrayBuffer.set(iv, SALT_LENGTH)
  arrayBuffer.set(encryptedArray, IV_AND_SALT_LENGTH)
  return arrayBuffer
}

export const decrypt = async ({
  arrayBuffer,
  password,
}: {
  arrayBuffer: Uint8Array
  password: string
}): Promise<string> => {
  const salt = arrayBuffer.slice(0, SALT_LENGTH)
  const iv = arrayBuffer.slice(SALT_LENGTH, IV_AND_SALT_LENGTH)
  const encryptedArray = arrayBuffer.slice(IV_AND_SALT_LENGTH)
  const decrypted = await crypto.subtle.decrypt(
    {
      name: ALGORITHM,
      iv,
    },
    await getEncryptionKey(password, salt),
    encryptedArray,
  )
  const { transactions } = deserialize(decrypted)
  return JSON.stringify(transactions)
}
