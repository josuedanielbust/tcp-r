const express = require('express')
const GIFEncoder = require('gif-encoder-2')
const { createCanvas, Image } = require('canvas')
const { createWriteStream, readdir, readFileSync } = require('fs')
const { promisify } = require('util')
const path = require('path')
const R = require('r-integration')

const port = 3000
const app = express()
app.set('view engine', 'pug')
app.use(express.static('results'))

async function createGif(dataset) {
  return new Promise(async resolveGif => {
    const imagesFolder = path.join(__dirname, 'results', dataset)
    const readdirAsync = promisify(readdir)
    const files = await readdirAsync(imagesFolder)
      .then((files) => files.filter((file) => file.includes('png')))

    const [width, height] = await new Promise(resolveImage => {
      const image = new Image()
      image.onload = () => resolveImage([image.width, image.height])
      image.src = path.join(imagesFolder, files[0])
    })

    const outputPath = path.join(__dirname, 'results', dataset, 'result.gif')
    const writeStream = createWriteStream(outputPath)
    writeStream.on('close', () => resolveGif())

    const encoder = new GIFEncoder(width, height)
    encoder.createReadStream().pipe(writeStream)
    encoder.start()
    encoder.setDelay(200)

    const canvas = createCanvas(width, height)
    const ctx = canvas.getContext('2d')

    for (const file of files) {
      console.log('Adding file:', file)
      await new Promise(resolveFrame => {
        const image = new Image()
        image.onload = () => {
          ctx.drawImage(image, 0, 0)
          encoder.addFrame(ctx)
          resolveFrame()
        }
        image.src = path.join(imagesFolder, file)
      })
    }

    encoder.finish()
  })
}

app.get('/gif/:id', async (req, res) => {
  const { id } = req?.params
  await createGif( id )
  res.send({ message: 'GIF Generated!', id })
})

app.get('/result/:id', async (req, res) => {
  const { id } = req?.params
  const gif = `/${ id }/result.gif`
  let data = null
  try {
    const txtData = readFileSync(`results/${id}/result.txt`, 'utf-8')
    data = txtData
      .split('\n', 4)
      .map(line => {
        return line.slice(5, -1).split(': ')
      })
  } catch(err) {
    console.log('Error', err)
  }
  res.render('gif', { id: id, gif: gif, data: data })
})

app.get('/r/:id', async (req, res) => {
  const { id } = req?.params
  // let result = R.executeRScript('./tcp.R')
  const result = await R.callMethodAsync('./tcp.R', 'main', { dataset: id }).then((result) => {
    console.log('result', result);
  }).catch((error) => {
    console.error('error', error);
  })
  //console.log(result)
  res.send({ id, result })
})

app.listen(port, () => {
  console.log(`App listening on port ${port}`)
})
