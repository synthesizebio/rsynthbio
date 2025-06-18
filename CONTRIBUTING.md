## How releases work 

An endpoint is created by a new branch in huggingface with a new model: https://huggingface.co/synthesizebio/2.0/blob/2.2/README.md 

Something like: 

```
API_URL = "https://ajumbleofletters.us-east-1.aws.endpoints.huggingface.cloud"
```

Platform team manually adds this to their database so that the api base url and endpoint ends up looking for something like: 

```
https://app.synthesize.bio/api/model/v2.0
```