import type { ErrorMessages } from './types'

const errorMessages: ErrorMessages = {
	required: () => 'This is required',
	alpha: () => 'This should contain only latin letters',
	sameAs: () => `This should be the same as field`,
	numeric: () => 'This should contain only digits',
	number: () => 'This should be a positive number',
	calc: () => 'Please use only numbers, + and - signs',
}

export default errorMessages
