-module(glemini_ffi).

-export([peer_certificate/1]).

peer_certificate(SslSocket) ->
    case ssl:peercert(SslSocket) of
        {ok, Cert} ->
            {ok, public_key:pkix_decode_cert(Cert, plain)};
        {error, Reason} ->
            {error, Reason}
    end.
