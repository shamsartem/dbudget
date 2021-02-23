import type { Validator } from './lib/types'
import { createValidateFunction } from './lib/validation'

export const asyncValidator: Validator = {
	name: 'asyncValidator',
	getErrorMessage: () => 'async length <2',
	validator: (val): Promise<boolean> => {
		return new Promise((resolve, reject) => {
			setTimeout(() => {
				resolve(val.length > 2)
			}, 2000)
		})
	},
}

export default createValidateFunction()
