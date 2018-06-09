#![allow(renamed_and_removed_lints)]

error_chain! {
    foreign_links {
        Io(::std::io::Error);
        Utf8(::std::str::Utf8Error);
        FromUtf8(::std::string::FromUtf8Error);
    }

    errors {
        InvalidData {}
        InvalidType(expected: Vec<::ConstructorNumber>, received: ::ConstructorNumber) {}
        BoxedAsBare {}
        ReceivedSendType {}
        UnsupportedLayer {}
        NoAuthKey {}
        NoSalts {}
        WrongAuthKey {}
        InvalidLength {}
        Unknown {}
        FactorizationFailure {}
        AuthenticationFailure {}
    }
}
