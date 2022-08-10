import JSZip from 'jszip'

import { sendToElm } from './elm'

const IV_LENGTH = 12
const SALT_LENGTH = 16
const IV_AND_SALT_LENGTH = IV_LENGTH + SALT_LENGTH

const ALGORITHM = 'AES-GCM'

const getEncryptionKey = async (
  password: string,
  salt: Uint8Array,
  encrypt = false,
): Promise<CryptoKey> => {
  const textEncoder = new TextEncoder()
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
    { name: ALGORITHM, length: 256 },
    false,
    [encrypt ? 'encrypt' : 'decrypt'],
  )
}

const DATA = 'data.json'

const prepareZip = (data: unknown): JSZip =>
  new JSZip().file(DATA, JSON.stringify(data))

const compressUint8Array = (data: unknown): Promise<Uint8Array> =>
  prepareZip(data).generateAsync({ type: 'uint8array' })

export const compressString = (data: unknown): Promise<string> =>
  prepareZip(data).generateAsync({ type: 'binarystring' })

export const decompress = async (
  ...args: Parameters<typeof JSZip.loadAsync>
): Promise<unknown> => {
  const zipFile = await JSZip.loadAsync(...args)
  const result: unknown = JSON.parse(
    (await zipFile.file(DATA)?.async('string')) ?? '',
  )
  return result
}

export const encrypt = async (
  data: unknown,
  password: string,
): Promise<Uint8Array> => {
  const salt = crypto.getRandomValues(new Uint8Array(SALT_LENGTH))
  const iv = crypto.getRandomValues(new Uint8Array(IV_LENGTH))
  const encrypted: unknown = await crypto.subtle.encrypt(
    {
      name: ALGORITHM,
      iv,
    },
    await getEncryptionKey(password, salt, true),
    await compressUint8Array(data),
  )

  if (!(encrypted instanceof ArrayBuffer)) {
    sendToElm('Toast', 'Incorrect data after encryption. Unrecoverable error')
    throw new Error('Incorrect data after encryption')
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

export const decrypt = async (
  arrayBuffer: Uint8Array,
  password: string,
): Promise<unknown> => {
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
    sendToElm('Toast', 'Decrypted data has wrong format. Unrecoverable error')
    throw new Error('Decrypted data has wrong format')
  }

  return decompress(decrypted)
}
