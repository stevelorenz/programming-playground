#[allow(dead_code, unused_macros)]

macro_rules! define_complex {
    () => {
        #[derive(Clone, Copy, Debug)]
        struct Complex<T> {
            /// Real portion of the complex number
            re: T,

            /// Imaginary portion of the complex number
            im: T,
        }
    };
}

mod first_cut {
    #[allow(dead_code, unused_macros)]
    #[derive(Clone, Copy, Debug)]
    struct Complex<T> {
        re: T,
        im: T,
    }

    // Implement operator overloading for the type Complex
    use std::ops::Add;

    impl<T> Add for Complex<T>
    where
        T: Add<Output = T>,
    {
        type Output = Self;

        fn add(self, rhs: Self) -> Self {
            return Complex {
                re: self.re + rhs.re,
                im: self.im + rhs.im,
            };
        }
    }

    use std::ops::Sub;
    impl<T> Sub for Complex<T>
    where
        T: Sub<Output = T>,
    {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self {
            return Complex {
                re: self.re - rhs.re,
                im: self.im - rhs.im,
            };
        }
    }

    use std::ops::Mul;
    impl<T> Mul for Complex<T>
    where
        T: Clone + Add<Output = T> + Sub<Output = T> + Mul<Output = T>,
    {
        type Output = Self;
        fn mul(self, rhs: Self) -> Self {
            Complex {
                re: self.re.clone() * rhs.re.clone() - (self.im.clone() * rhs.im.clone()),
                im: self.im * rhs.re + self.re * rhs.im,
            }
        }
    }

    #[test]
    fn try_it_out() {
        let z = Complex { re: 1, im: 2 };
        let c = Complex { re: 3, im: 4 };

        _ = z * z + c;
        // std::mem::forget(z);
    }

    impl<T: PartialEq> PartialEq for Complex<T> {
        fn eq(&self, other: &Complex<T>) -> bool {
            return self.re == other.re && self.im == other.im;
        }
    }

    #[test]
    fn test_complex_eq() {
        let x = Complex { re: 5, im: 2 };
        let y = Complex { re: 2, im: 5 };
        assert_eq!(x * y, Complex { re: 0, im: 29 });
    }
}

#[test]
fn test_string() {
    let spacey = "man hat tan";
    let spaceless: String = spacey.chars().filter(|c| !c.is_whitespace()).collect();
    assert_eq!(spaceless, "manhattan");

    let haystack = "One fine day, in the middle of the night";
    assert_eq!(haystack.find(","), Some(12));
    assert_eq!(haystack.find("night"), Some(35));
    assert_eq!(haystack.find(char::is_whitespace), Some(3));
}

mod formatting {
    #![allow(dead_code)]
    #[test]
    fn complex() {
        #[derive(Copy, Clone, Debug)]
        struct Complex {
            re: f64,
            im: f64,
        }

        let third = Complex {
            re: -0.5,
            im: f64::sqrt(0.75),
        };
        println!("{:?}", third);

        use std::fmt;

        impl fmt::Display for Complex {
            fn fmt(&self, dest: &mut fmt::Formatter) -> fmt::Result {
                let im_sign = if self.im < 0.0 { '-' } else { '+' };
                return write!(dest, "{} {} {}i", self.re, im_sign, f64::abs(self.im));
            }
        }

        let one_twenty = Complex {
            re: -0.5,
            im: 0.866,
        };
        assert_eq!(format!("{}", one_twenty), "-0.5 + 0.866i");
    }
}
