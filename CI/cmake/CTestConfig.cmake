set(CTEST_PROJECT_NAME "JEDI")
set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
set(CTEST_DROP_METHOD "https")
set(CTEST_DROP_SITE "cdash.jcsda.org")
set(CTEST_DROP_SITE_CDASH TRUE)
set(CTEST_USE_LAUNCHERS 1)
set(ENV{CTEST_USE_LAUNCHERS_DEFAULT} 1)
set(CTEST_LABELS_FOR_SUBPROJECTS iodaconv)
set(CTEST_DROP_LOCATION "/submit.php?project=ioda")
