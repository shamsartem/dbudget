<Header />
{#if loading}
	Loading...
{:else}
	<label class="button">
		<span>Upload</span>
		<input
			bind:this="{input}"
			on:change="{handleChange}"
			type="file"
			class="visuallyHidden"
		/>
	</label>
{/if}

<script lang="ts">
	import Header from '../components/header.svelte'
	import { createFileInputChangeHandler } from '../lib/transaction'

	let input: HTMLInputElement
	let loading: boolean = false

	const changeHandler = createFileInputChangeHandler(
		(e) => {
			loading = false
			console.log(e)
		},
		() => {
			loading = false
		},
	)

	const handleChange = (
		e: Event & {
			currentTarget: EventTarget & HTMLInputElement
		},
	) => {
		loading = true
		changeHandler(e)
	}
</script>
