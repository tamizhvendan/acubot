type HttpMethod = Get | Post | Put

type Request = {
    HttpMethod : HttpMethod
    Path : string
}

type StatusCode = Ok | BadRequest | NotFound

let toStatusCode code =
    match code with
    | Ok -> 200
    | BadRequest -> 400
    | NotFound -> 404