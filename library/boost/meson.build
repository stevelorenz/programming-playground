project('boost', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

thread_dep = dependency('threads')
boost_headr_only_dep = dependency('boost')
boost_link_dep = dependency('boost', modules : ['program_options', 'coroutine', 'system', 'log'], required: true)

executable('pool', 'pool.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('container', 'container.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('msm', 'msm.cpp',
  dependencies : [thread_dep, boost_headr_only_dep])

executable('string_test', 'string_test.cpp',
  dependencies : [thread_dep, boost_headr_only_dep, boost_link_dep])

executable('coroutines', 'coroutines.cpp',
  dependencies : [thread_dep, boost_headr_only_dep, boost_link_dep])

executable('log', 'log.cpp',
  dependencies : [thread_dep, boost_headr_only_dep, boost_link_dep])
