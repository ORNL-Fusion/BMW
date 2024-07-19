target_sources(bmw
               PRIVATE
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/bmw_commandline_parser.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/bmw_context.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/bmw_parallel_context.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/bmw_state_flags.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/m_grid.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/primed_grid.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/siesta_file.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/vmec_file.f>
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/unprimed_grid.f>
)

target_sources(xbmw
               PRIVATE
               $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/bmw.f>
)
