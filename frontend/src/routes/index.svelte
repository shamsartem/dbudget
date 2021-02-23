{#if transactionsToShow && transactionsToShow.length}
	<VirtualList items="{transactionsToShow}" itemHeight="{70}" let:item>
		<div class="header" slot="top">
			<Header />
			<SInput bind:field="{form.searchQuery}" />
		</div>
		<div slot="item" class="item" class:_isIncome="{!item.isExpense}">
			<div aria-hidden="true" class="itemSection">
				<div>{item.date}</div>
				<div>{item.category}</div>
			</div>
			<div aria-hidden="true" class="itemSection">
				<div>{item.name}</div>
				<div>{item.priceToShow}</div>
			</div>
			<a class="fullSize" href="#{transactionId}">
				<span class="visuallyHidden">
					{item.name}
					{item.priceToShow}
					{item.date}
					{item.category}
				</span>
			</a>
		</div>
	</VirtualList>
{:else}
	<Header />
	<div class="nothing">Nothing found</div>
{/if}
<Transaction id="{transactionId}" bind:isVisible="{isTransactionVisible}" />
<button class="roundButton" on:click="{addTransaction}">+</button>

<script lang="ts">
	import { transactionsArray } from '../stores/transactions'
	import VirtualList from '../components/virtual-list.svelte'
	import SInput from '../components/s-input.svelte'
	import Transaction from '../components/transaction.svelte'
	import Header from '../components/header.svelte'
	import type { Field } from '../validation/lib/types'
	let transactionId: null | string = null
	let isTransactionVisible = false

	const addTransaction = () => {
		isTransactionVisible = true
	}

	let form: {
		searchQuery: Field
	} = {
		searchQuery: {
			value: '',
		},
	}

	$: transactionsToShow = form.searchQuery.value
		? $transactionsArray.filter((t) =>
				Object.entries(t)
					.reduce<string>((acc, [key, val]) => {
						if (['isExpense', 'amount'].includes(key)) {
							return acc
						}
						return acc ? `${acc} ${val}` : `${val}`
					}, '')
					.includes(`${form.searchQuery.value}`),
		  )
		: $transactionsArray

	const handleHashChange = () => {
		const { hash } = window.location
		const id = hash.substr(1)

		if (!id) {
			return
		}
	}
</script>

<style lang="postcss">
	.header {
		display: flex;
		position: sticky;
		z-index: var(--z-default);
		top: 0;
		flex-direction: column;
		align-items: center;
		width: 100%;
		background-color: var(--c-gray-1);

		& > :global(.search) {
			width: 100%;
			max-width: var(--max-page-width);
			padding: var(--space-md);
			border-bottom: 1px solid var(--c-gray-4);
		}
	}

	.item {
		display: flex;
		position: relative;
		flex-direction: column;
		align-items: center;
		justify-content: space-between;
		width: 100%;
		max-width: var(--max-page-width);
		height: 70px;
		padding: var(--space-md);
		border-bottom: 1px solid var(--c-gray-4);

		&._isIncome {
			background-color: var(--c-gray-2);
		}
	}

	.itemSection {
		display: flex;
		justify-content: space-between;
		width: 100%;
	}

	.nothing {
		width: 100%;
		padding-top: 20px;
		text-align: center;
	}
</style>
