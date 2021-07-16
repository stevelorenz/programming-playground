project('boost', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

thread_dep = dependency('threads')
spdlog_dep = dependency('spdlog', required: true)

executable('test_spdlog', 'test_spdlog.cpp',
  dependencies : [thread_dep, spdlog_dep])
