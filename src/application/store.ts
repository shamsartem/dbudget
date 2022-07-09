import type { JSONSchemaType } from 'ajv'

import { ajv } from './ajv'

export type Cred = {
  password: string
  username: string
  deviceName: string
}

const credSchema: JSONSchemaType<Cred> = {
  type: 'object',
  properties: {
    password: { type: 'string' },
    username: { type: 'string' },
    deviceName: { type: 'string' },
  },
  required: ['password', 'username', 'deviceName'],
}

export const validateCred = ajv.compile(credSchema)

export const store: { cred: null | Cred } = { cred: null }
