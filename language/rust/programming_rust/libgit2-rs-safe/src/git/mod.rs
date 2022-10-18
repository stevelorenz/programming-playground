mod raw;

use std::error;
use std::fmt;
use std::result;

#[derive(Debug)]
pub struct Error {
    code: i32,
    message: String,
    class: i32,
}

/// Implement common traits for the error type
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        // Just return the error string from the libgit2
        return self.message.fmt(f);
    }
}

impl From<String> for Error {
    fn from(message: String) -> Error {
        Error {
            code: -1,
            message,
            class: 0,
        }
    }
}

// NulError is what `CString::new` returns if a string
// has embedded zero bytes.
impl From<std::ffi::NulError> for Error {
    fn from(e: std::ffi::NulError) -> Error {
        Error {
            code: -1,
            message: e.to_string(),
            class: 0,
        }
    }
}

// Use default methods
impl error::Error for Error {}

pub type Result<T> = result::Result<T, Error>;

use std::ffi::CStr;
use std::os::raw::c_int;

fn check(code: c_int) -> Result<c_int> {
    if code >= 0 {
        return Ok(code);
    }

    unsafe {
        let error = raw::giterr_last();
        let message = CStr::from_ptr((*error).message)
            .to_string_lossy()
            .into_owned();
        return Err(Error {
            code: code as i32,
            message,
            class: (*error).klass as i32,
        });
    }
}

pub struct Repository {
    // A pointer to a live `git_repository` object
    raw: *mut raw::git_repository,
}

use std::path::Path;
use std::ptr;

impl Repository {
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Repository> {
        ensure_initialized();

        let path = path_to_cstring(path.as_ref())?;
        let mut repo = ptr::null_mut();
        unsafe {
            check(raw::git_repository_open(&mut repo, path.as_ptr()))?;
        }
        return Ok(Repository { raw: repo });
    }
}

fn ensure_initialized() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| unsafe {
        check(raw::git_libgit2_init()).expect("Failed to init libgit2");
        assert_eq!(libc::atexit(shutdown), 0);
    });
}

extern "C" fn shutdown() {
    unsafe {
        if let Err(e) = check(raw::git_libgit2_shutdown()) {
            eprintln!("{}", e);
            std::process::abort();
        }
    }
}

impl Drop for Repository {
    fn drop(&mut self) {
        unsafe {
            raw::git_repository_free(self.raw);
        }
    }
}

use std::ffi::CString;

#[cfg(unix)]
fn path_to_cstring(path: &Path) -> Result<CString> {
    // The `as_bytes` method exists only on Unix-like systems.
    use std::os::unix::ffi::OsStrExt;

    Ok(CString::new(path.as_os_str().as_bytes())?)
}

pub struct Oid {
    pub raw: raw::git_oid,
}

use std::mem;
use std::os::raw::c_char;

impl Repository {
    pub fn reference_name_to_id(&self, name: &str) -> Result<Oid> {
        let name = CString::new(name)?;
        unsafe {
            let oid = {
                let mut oid = mem::MaybeUninit::uninit();
                check(raw::git_reference_name_to_id(
                    oid.as_mut_ptr(),
                    self.raw,
                    name.as_ptr() as *const c_char,
                ))?;
                oid.assume_init()
            };
            Ok(Oid { raw: oid })
        }
    }
}

use std::marker::PhantomData;

pub struct Commit<'repo> {
    raw: *mut raw::git_commit,
    _marker: PhantomData<&'repo Repository>,
}

impl Repository {
    pub fn find_commit(&self, oid: &Oid) -> Result<Commit> {
        let mut commit = ptr::null_mut();
        unsafe {
            check(raw::git_commit_lookup(&mut commit, self.raw, &oid.raw))?;
        }
        Ok(Commit {
            raw: commit,
            _marker: PhantomData,
        })
    }
}

impl<'repo> Drop for Commit<'repo> {
    fn drop(&mut self) {
        unsafe {
            raw::git_commit_free(self.raw);
        }
    }
}

impl<'repo> Commit<'repo> {
    pub fn author(&self) -> Signature {
        unsafe {
            Signature {
                raw: raw::git_commit_author(self.raw),
                _marker: PhantomData,
            }
        }
    }

    pub fn message(&self) -> Option<&str> {
        unsafe {
            let message = raw::git_commit_message(self.raw);
            char_ptr_to_str(self, message)
        }
    }
}

pub struct Signature<'text> {
    raw: *const raw::git_signature,
    _marker: PhantomData<&'text str>,
}

impl<'text> Signature<'text> {
    /// Return the author's name as a `&str`,
    /// or `None` if it is not well-formed UTF-8.
    pub fn name(&self) -> Option<&str> {
        unsafe { char_ptr_to_str(self, (*self.raw).name) }
    }

    /// Return the author's email as a `&str`,
    /// or `None` if it is not well-formed UTF-8.
    pub fn email(&self) -> Option<&str> {
        unsafe { char_ptr_to_str(self, (*self.raw).email) }
    }
}

unsafe fn char_ptr_to_str<T>(_owner: &T, ptr: *const c_char) -> Option<&str> {
    if ptr.is_null() {
        return None;
    } else {
        CStr::from_ptr(ptr).to_str().ok()
    }
}
