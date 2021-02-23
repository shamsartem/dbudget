<CommonStyles />
<main>
	<slot />
</main>
<Backdrop />

<script lang="ts">
	import { goto } from '@sapper/app'
	import { onMount } from 'svelte'
	import { is_client } from 'svelte/internal'
	import Backdrop from '../components/backdrop.svelte'
	import { emitUsername } from '../lib/socket'
	import { credentials } from '../stores/credentials'
	import CommonStyles from '../styles/common.svelte'
	export let segment: string

	$: redirect = () => {
		if ($credentials.username === null) {
			goto('/log-in')
		} else if (segment === 'log-in') {
			goto('/')
		}
	}

	onMount(() => $credentials.username && emitUsername($credentials.username))

	$: if (is_client) {
		redirect()
	}
</script>

<style lang="postcss">
	main {
		display: flex;
		flex-direction: column;
		flex-grow: 1;
		align-items: center;
		width: 100%;
		height: 100vh;
	}
</style>
