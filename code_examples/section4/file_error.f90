integer :: ierr
character(len=100) :: errmsg

open(unit=10, file='data.txt', status='old', iostat=ierr, iomsg=errmsg)

if (ierr .ne. 0) then
    print *, 'Error opening file: ', trim(errmsg)
    stop
end if
