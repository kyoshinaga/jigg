# Keras Parser

## Requirements
jigg.ml.keras.KerasParser requires files listed in below:

- Model file (e.g. ssplit_model.h5)
- Lookup table (e.g. jpnLookup.json)

### Model file
- File format: HDF5
- Output Class: BIO

### Lookup table
- Filed construction
  - `_lookup`
    - `_key2id`: Character to ID
    - `_id2key`: ID to character

#### Example
```json
{"_lookup":{
    "_key2id": {
        "あ": "1",
        "い": "2"
        },
    "_id2key": {
        "1": "あ",
        "2": "い"
    }
  }
}
```
