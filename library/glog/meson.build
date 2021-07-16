project('test_glog', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

thread_dep = dependency('threads')
glog_dep = dependency('libglog', required: true)
fmt_dep = dependency('fmt', required: true)

executable('test_glog', 'test_glog.cpp',
  dependencies : [thread_dep, glog_dep, fmt_dep])
