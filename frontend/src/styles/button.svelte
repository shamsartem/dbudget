<style global lang="postcss">
	.button {
		display: flex;
		justify-content: center;
		padding: 10px;
		transition: background-color var(--t);
		border-radius: var(--bdrs-md);
		background-color: var(--c-gray-3);
		cursor: pointer;

		&:hover {
			background-color: var(--c-gray-2);
		}
	}

	.roundButton {
		background-color: var(--c-gray-2);
		position: fixed;
		right: 15px;
		bottom: 15px;
		display: flex;
		justify-content: center;
		align-items: center;
		width: 56px;
		height: 56px;
		border-radius: 50%;
		cursor: pointer;
		transition: box-shadow var(--t);
		box-shadow: 0 2px 2px 0 rgba(0, 0, 0, 0.14),
			0 3px 1px -2px rgba(0, 0, 0, 0.12), 0 1px 5px 0 rgba(0, 0, 0, 0.2);
		&:hover {
			box-shadow: 0 3px 3px 0 rgba(0, 0, 0, 0.14),
				0 1px 7px 0 rgba(0, 0, 0, 0.12), 0 3px 1px -1px rgba(0, 0, 0, 0.2);
		}
	}
</style>
