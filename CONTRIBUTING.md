## How releases work

An endpoint is created by a new branch in huggingface with a new model: https://huggingface.co/synthesizebio/2.0/blob/2.2/README.md

Something like:

```
API_URL = "https://ajumbleofletters.us-east-1.aws.endpoints.huggingface.cloud"
```

Platform team manually adds this to their database by model semantic version. The proxy API, used by this package and located at https://app.synthesize.bio/api/model/vX.X will look up the model version to get the proper huggingface endpoint.

If we're not ready to release this model in the platform yet, we should ensure that the new DB row has `is_deleted`=`TRUE`.

If this is a major version release or the request or response payload has changed, we should update the docs located at https://app.synthesize.bio/docs.
