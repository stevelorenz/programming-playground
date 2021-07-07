project('asio', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

thread_dep = dependency('threads')
boost_headr_only_dep = dependency('boost')
boost_link_dep = dependency('boost', modules : ['program_options', 'coroutine'], required: true)

executable('timer', 'timer.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('web_get', 'web_get.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('tcp_server', 'tcp_server.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('coroutines', 'coroutines.cpp',
  dependencies : [thread_dep, boost_headr_only_dep, boost_link_dep])
