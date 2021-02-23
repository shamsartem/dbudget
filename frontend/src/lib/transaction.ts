import Papa, { UnparseObject } from 'papaparse'

import { TRANSACTIONS } from '../const'

export type Transaction = {
	isExpense: string
	date: string
	category: string
	name: string
	price: string
	amount: string
	description: string
	currency: string
	lastChanged: string
	id: string
}

export type Decimals = { [key: string]: number }

const calcToNumber = (calc: string, decimals?: number): number =>
	calc
		.split('+')
		.reduce((acc, n) => (decimals ? acc + +n * 10 ** decimals : acc + +n), 0)

const getNumberOfDecimals = (n: string) => n.split('.')[1]?.length || 0

const getMaxNumberOfDecimals = (calc: string): number => {
	const numbers = calc.split('+')

	return numbers.reduce((maxNumberOfDecimals, n) => {
		const numberOfDecimals = getNumberOfDecimals(n)
		return numberOfDecimals > maxNumberOfDecimals
			? numberOfDecimals
			: maxNumberOfDecimals
	}, 0)
}

export const getDecimals = (transactions: Array<Transaction>) =>
	transactions.reduce<Decimals>((acc, { price, currency }) => {
		const maxNumberOfDecimals = getMaxNumberOfDecimals(price)
		if (acc[currency] === undefined) {
			acc[currency] = 0
		}
		if (acc[currency] < maxNumberOfDecimals) {
			acc[currency] = maxNumberOfDecimals
		}
		return acc
	}, {})

export const getPriceToShow = (
	{ price, amount, currency }: Transaction,
	decimals: number,
) => {
	const multiplier = 10 ** decimals
	const rounded =
		Math.round(
			calcToNumber(price, decimals) * calcToNumber(amount) * multiplier,
		) / multiplier
	const numberOfDecimals = getNumberOfDecimals(rounded.toString())
	const numberOfMissingZeroes = decimals - numberOfDecimals
	const zeroes =
		numberOfDecimals === decimals
			? ''
			: new Array(numberOfMissingZeroes).fill('0').join('')
	const dot = numberOfDecimals === 0 ? '.' : ''
	return `${rounded}${dot}${zeroes} ${currency}`
}

type FileError = { fileError?: string }
type ParseResult<T> = Papa.ParseResult<T> | FileError

const parse = <T>(
	input: string | NodeJS.ReadableStream | File,
	config: Papa.ParseConfig<unknown>,
): Promise<ParseResult<T>> =>
	new Promise((resolve) => {
		Papa.parse(input, {
			...config,
			complete(res: Papa.ParseResult<T>) {
				resolve(res)
			},
			error: () =>
				resolve({
					fileError: 'File error',
				}),
		})
	})

type ValidationResult = { errors: Array<string> } | { csv: string }

const isFileError = <T>(
	parseResult: ParseResult<T>,
): parseResult is FileError => 'fileError' in parseResult

const validateAndPrepareCSV = async (csv: File): Promise<ValidationResult> => {
	const nowTimestamp = new Date().toJSON()
	const parseResult = await parse<Transaction>(csv, {
		header: true,
	})

	if (isFileError(parseResult)) {
		return { errors: [parseResult.fileError] }
	}

	const { errors, data } = parseResult

	if (errors.length) {
		return {
			errors: errors.map(({ row, message }) => `In row ${row}: ${message}`),
		}
	}

	return { csv: Papa.unparse(data) }
}

export const createFileInputChangeHandler = (
	errorCallback: (error: Array<string>) => void,
	successCallback: () => void,
) => async (
	e: Event & {
		currentTarget: EventTarget & HTMLInputElement
	},
) => {
	const csv = e.currentTarget.files[0]
	const validationResult = await validateAndPrepareCSV(csv)

	if ('errors' in validationResult) {
		errorCallback(validationResult.errors)
		return
	}

	// localStorage.setItem(TRANSACTIONS, `${validationResult.csv}`)
	successCallback()
}
