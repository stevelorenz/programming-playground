#![allow(dead_code)]

mod my_ascii {

    // This is a tuple struct
    #[derive(Debug, PartialEq, Eq)]
    pub struct Ascii(
        // This must hold only well-formed ASCII text:
        // bytes from `0` to `0x7f`.
        Vec<u8>,
    );

    // This should implement the std::error::Error trait, here not, just for testing
    #[derive(Debug, PartialEq, Eq)]
    pub struct NotAsciiError(Vec<u8>);

    impl Ascii {
        pub fn from_bytes(bytes: Vec<u8>) -> Result<Ascii, NotAsciiError> {
            if bytes.iter().any(|&byte| !byte.is_ascii()) {
                return Err(NotAsciiError(bytes));
            }
            return Ok(Ascii(bytes));
        }
    }

    impl From<Ascii> for String {
        fn from(ascii: Ascii) -> String {
            unsafe {
                return String::from_utf8_unchecked(ascii.0);
            }
        }
    }

    impl Ascii {
        /// # Safety
        ///
        /// The caller must ensure that `bytes` contains only ASCII
        /// characters: bytes no greater than 0x7f. Otherwise, the effect is
        /// undefined.
        pub unsafe fn from_bytes_unchecked(bytes: Vec<u8>) -> Ascii {
            Ascii(bytes)
        }
    }
}

#[test]
fn good_ascii() {
    use my_ascii::Ascii;

    let bytes: Vec<u8> = b"ASCII and ye shall receive".to_vec();

    let ascii: Ascii = Ascii::from_bytes(bytes).unwrap();
    let string = String::from(ascii);

    assert_eq!(string, "ASCII and ye shall receive");
}

#[test]
fn bad_ascii() {
    use my_ascii::Ascii;

    // Imagine that this vector is the result of some complicated process
    // that we expected to produce ASCII. Something went wrong!
    let bytes = vec![0xf7, 0xbf, 0xbf, 0xbf];

    let ascii = unsafe {
        // This unsafe function's contract is violated
        // when `bytes` holds non-ASCII bytes.
        Ascii::from_bytes_unchecked(bytes)
    };

    let bogus: String = ascii.into();

    // `bogus` now holds ill-formed UTF-8. Parsing its first character produces
    // a `char` that is not a valid Unicode code point. That's undefined
    // behavior, so the language doesn't say how this assertion should behave.
    assert_eq!(bogus.chars().next().unwrap() as u32, 0x1fffff);
}
