import { req } from './common'
import type { Validator } from '../types'

const required: Validator = {
	name: 'required',
	validator: (value) => {
		if (typeof value === 'string') {
			return req(value.trim())
		}
		return req(value)
	},
	getErrorMessage: () => 'this is required',
}

export default required
