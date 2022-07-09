class AssertError extends Error {
  constructor(message = '', args?: ErrorOptions) {
    super(message, args)
    this.message = `Assert error: ${message}`
  }
}

export const isObject = (unknown: unknown): unknown is object =>
  unknown !== null && typeof unknown === 'object'

export const hasKey = <K extends string>(
  unknown: unknown,
  key: K,
): unknown is Record<K, unknown> => isObject(unknown) && key in unknown

export const hasKeys = <K extends string>(
  unknown: unknown,
  ...keys: Array<K>
): unknown is Record<K, unknown> =>
  keys.every((key): boolean => hasKey(unknown, key))

export const assertHasKey: <K extends string>(
  unknown: unknown,
  key: K,
  msg: string,
) => asserts unknown is Record<K, unknown> = <K extends string>(
  unknown: unknown,
  key: K,
  msg: string,
): asserts unknown is Record<K, unknown> => {
  if (!hasKey(unknown, key)) {
    throw new AssertError(msg)
  }
}

export const isUint8Array = (unknown: unknown): unknown is Uint8Array =>
  unknown instanceof Uint8Array

export const assertIsUint8Array: (
  unknown: unknown,
  msg: string,
) => asserts unknown is Uint8Array = (
  unknown: unknown,
  msg: string,
): asserts unknown is Uint8Array => {
  if (!isUint8Array(unknown)) {
    throw new AssertError(msg)
  }
}
