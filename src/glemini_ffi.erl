-module(glemini_ffi).

-export([peer_certificate/1]).

peer_certificate(SslSocket) ->
    case ssl:peercert(SslSocket) of
        {ok, Cert} ->
            {'OTPCertificate', Data, _, _} = public_key:pkix_decode_cert(Cert, otp),
            {
                'OTPTBSCertificate',
                _Version,
                _Serial,
                _Signature,
                Issuer,
                _Validity,
                Subject,
                _PubKey,
                _IssuerUID,
                _SubjectUID,
                _Extension
            } = Data,
            {ok, {
                certificate,
                get_rdn_sequence_value(Issuer),
                get_rdn_sequence_value(Subject),
                Cert
            }};
        {error, Reason} ->
            {error, Reason}
    end.

get_rdn_sequence_value({rdnSequence, [[{'AttributeTypeAndValue', _, Value} | _] | _]}) ->
    to_string(Value).

to_string(L) when is_list(L) -> list_to_binary(L);
to_string({utf8String, B}) when is_binary(B) -> B.
