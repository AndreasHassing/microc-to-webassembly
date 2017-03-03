// fetch server dependencies
const express    = require('express')
const app        = express()
const bodyParser = require('body-parser')

// remove x-powered-by from headers
app.disable('x-powered-by')

// parse received bodies as json
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())

// server setup
const port   = process.env.PORT || 8080
const router = express.Router()

// routes
router.post('/compile', (req, res) => {
  let language = req.body.language
  let code = req.body.code

  // local logging (for metrics and debugging)
  console.log(`compiling from ${language} to webassembly:\n${code}\n`)

  // compile the code here (refer to sandbox.js)
  // ...

  // respond with compiled wasm and wast
  res.json({
    message: 'finished compiling :-)',
    wasm: '<wasm binary code goes here>',
    wast: '<wast s-expression goes here>'
  })
})

// explain how to use api
router.get('/', (req, res) => {
  res.json({ message: 'usage: POST /api/compile => language: "MicroC", code: "<code here>"' })
})

// use router at path
app.use('/api', router)

// start listening asynchronously
app.listen(port)

// log server start
console.log(`To WebAssembly Compiler WebAPI running on port ${port}`)
