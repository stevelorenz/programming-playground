project('test_openssl', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

thread_dep = dependency('threads')
openssl_dep = dependency('openssl', required: true)

executable('test_openssl', 'test_openssl.cpp',
  dependencies : [thread_dep, openssl_dep])

executable('password', 'password.cpp',
  dependencies : [thread_dep, openssl_dep])
