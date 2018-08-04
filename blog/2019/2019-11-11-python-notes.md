---
title: Notes on Python
tags: python
---

## Setting log level dynamically

If you want to change the log level for one iteration of a loop or one call of a function, it can be done by calling `setLevel` on the current logger instance.

~~~{.python}
import logging

logging.error("This statement will be shown")

logging.debug("This will not be shown")

logging.getLogger().setLevel("DEBUG")

logging.debug("This debug statement will be shown")

logging.getLogger().setLevel("ERROR")

logging.debug("This debug statement will be hidden")
~~~

This results in

~~~
ERROR:root:This statement will be shown
DEBUG:root:This debug statement will be shown
~~~

### Related reading

1. Logging module documentation - <https://docs.python.org/3.8/library/logging.html>
2. Logging HOWTO - <https://docs.python.org/3.8/howto/logging.html>
