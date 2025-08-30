const lsAr = ['docUrl', 'deviceName', 'server'] as const

type LocalStorageKey = (typeof lsAr)[number]

export function lsGet(key: LocalStorageKey) {
  return localStorage.getItem(key)
}

export function lsGetWithDefault(key: LocalStorageKey, defaultValue: string) {
  const item = localStorage.getItem(key)

  if (item === null) {
    localStorage.setItem(key, defaultValue)
    return defaultValue
  }

  return item
}

export function lsSet(key: LocalStorageKey, value: string) {
  localStorage.setItem(key, value)
}
