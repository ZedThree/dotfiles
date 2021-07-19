add-auto-load-safe-path /home/peter/Codes/IDAM/latest/test/source/plugins/newhdf5/.gdbinit
add-auto-load-safe-path /home/peter/Codes/IDAM/latest/test/source/plugins/newcdf4/.gdbinit
add-auto-load-safe-path /home/peter/Codes/IDAM/latest/source/wrappers/fortran/.gdbinit
add-auto-load-safe-path /home/peter/Codes/IDAM/latest/source/wrappers/c++/.gdbinit
set print pretty
set print null-stop
catch throw
set disassembly-flavor intel

# enable history, probably
set history filename ~/.gdb_history
set history save
set history size unlimited

set build-id-verbose 0
set print inferior-events off

set max-completions 20

python
import sys
sys.path.insert(0, '/usr/share/gcc-10/python')
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
end
