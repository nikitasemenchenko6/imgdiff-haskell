# imgdiff-haskell

`imgdiff` - cli tool to calculate difference between images.
Results are stable for next transformations: "scale-up/scale-down", color adjustment, quality reduce
Returns % of difference

Thanks https://github.com/Nr90/imgsim/ for Average and Difference hash calculations 

### Install

```bash
stack build
```

### Use

```bash
stack exec -- imgdiff-haskell-exe --f1="./test-png-original.png" --f2="./test-png-damaged.png"
Diffeernce: 3.125%
```

### Features

[x] JPEG
[] PNG 
[x] Local files 
[] Download file from Url

### License

`imgdiff-haskell` is [MIT licensed](./LICENSE)