blessed = require('blessed')
Elm = require('./main.js')

manager = Elm.Main.worker()

screen =
  blessed.screen
    smartCRS: true
    cursor:
      artificial: true
      shape:
        ch: '#'
      blink: true

screen.render()

screen.key ['escape', 'q', 'C-c'], (ch, key) =>
  return process.exit 0

manager.ports.updates.subscribe (transactions) =>
  renderIndex transactions.index

renderIndex = (transactions) =>
  rows = transactionsToTable transactions

  selectedRowId = (table) ->
    tableRowId(rows[table.getItemIndex table.selected])

  table=
    blessed.ListTable
      rows: rows
      parent: screen
      keys: true
      left: 0,
      border: 'line',
      align: 'center',
      width: '90%',
      height: '60%',
      vi: true,
      name:'table'
      style:
        bg: 'blue'
        fg: 'black'

  screen.key ['d'], (ch, key) =>
    text = blessed.text
      parent: screen
      top: '99%'
      left: 'left'
      content: "Do you want to delete this transaction? y/n"
    text.focus()

    screen.key ['y'], (ch, key) =>
      console.log selectedRowId table
      text.detach()
      table.focus()
      screen.unkey ['y']
      screen.unkey ['n']

    screen.key ['n'], (ch, key) =>
      table.focus()
      text.detach()
      screen.unkey ['y']
      screen.unkey ['n']

transactionsToTable = (transactions) ->
  [["Date", "Payee", "Category", "Account", "Amount", "Memo"]]
    .concat transactions.map (t) =>
      tableRow t

tableRow = (t) ->
  [t.date, t.payee, t.category, t.account, t.amount.toString(), displayNull(t.memo), t.id]

tableRowId = (row) ->
  row[6]

displayNull = (str) ->
  if str == null
    ""
  else
    str

