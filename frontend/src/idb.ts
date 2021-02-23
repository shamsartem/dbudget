import { openDB } from 'idb'
import { is_client, TRANSACTIONS } from './const'

const dbPromise =
	is_client &&
	openDB(TRANSACTIONS, 1, {
		upgrade(db) {
			if (!db.objectStoreNames.contains(TRANSACTIONS)) {
				db.createObjectStore(TRANSACTIONS)
			}
		},
	})

export const getTransactionFromDB = async () => {
	const db = await dbPromise
	const transaction = db.transaction(TRANSACTIONS, 'readonly')
	const store = transaction.objectStore(TRANSACTIONS)
	return store.get(TRANSACTIONS)
}
