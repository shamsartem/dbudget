import { LionCombobox } from '@lion/combobox'
import { LionOption } from '@lion/listbox'
import { customElement } from 'lit/decorators.js'

@customElement('db-combobox')
export class DBudgetComboboxEl extends LionCombobox {}

@customElement('db-option')
export class DBudgetOption extends LionOption {}
