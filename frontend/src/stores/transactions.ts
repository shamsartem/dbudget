import { derived, writable } from 'svelte/store'
import { getDecimals, getPriceToShow } from '../lib/transaction'
import type { Transaction } from '../lib/transaction'

export const transactions = writable<{
	[uuid: string]: Transaction
}>(null)

export const transactionsArray = derived(
	transactions,
	($transactions) =>
		$transactions &&
		Object.entries($transactions).map(([uuid, transaction]) => ({
			uuid,
			...transaction,
		})),
)

export const decimals = derived(
	transactionsArray,
	($transactionsArray) => $transactionsArray && getDecimals($transactionsArray),
)

export const transactionsWithPriceToShowArray = derived(
	[transactionsArray, decimals],
	($transactionsArray, $decimals) =>
		$transactionsArray &&
		$transactionsArray.map((t) => ({
			...t,
			priceToShow: getPriceToShow(t, $decimals[t.currency]),
		})),
)
