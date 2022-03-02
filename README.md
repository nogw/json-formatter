# json-formatter

```json
{"a": "b","c": {"d": {"e": "f","g": "h","i": "j","k": {"l": ["m","n","o"]}}}}
```

```sh
Prelude> prettyJsonFile "input.json"
```

```json
{
	"a": "b",
	"c": {
		"d": {
			"e": "f",
			"g": "h",
			"i": "j",
			"k": {
				"l": [
					"m",
					"n",
					"o"
				]
			}
		}
	}
}
```