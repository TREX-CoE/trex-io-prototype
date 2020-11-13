#!/usr/bin/env python3
import os, sys
import re

def read_config(filename):
    """ Reads the config file and returns a dictionary where keys are
    groups and values are lists of items within the group.
    The items are tuples of (item_name, type, dimensions)."""

    file = open(filename,'r')
    lines = [(x,filename) for x in file.readlines()]
    try:
        if lines[-1] != '':
            lines.append( ('', filename) )
    except:
        pass
    file.close()

    groups = {}
    group = None
    my_list = []
    for line, filename in lines:
        line = line.lower()
        try:
            if len(line.strip()) == 0:
                groups[group] = my_list
            elif line[0] != ' ':  # New group
                group = line.strip()
                if group in list(groups.keys()):
                    my_list = groups[group]
                else:
                    my_list = []
            else:
                line = line.replace("double precision","double_precision")
                buffer = line.split()
                if len(buffer) == 2:
                    buffer += ["()"]
                else:
                    buffer[2] = re.sub(r"\)",r',)',buffer[2])
                if buffer[1] == "double_precision":
                    buffer[1] = "double precision"
                buffer[2] = re.sub(r"([a-zA-Z0-9_\-\+*/]+)",r'"\1"',buffer[2])
                buffer[2] = eval(buffer[2])
                my_list.append(tuple(buffer))
        except:
            import sys, time
            print('', file=sys.stderr)
            print('*'*80, file=sys.stderr)
            print('Error in TREXIO config file '+filename+' :', file=sys.stderr)
            print(line, file=sys.stderr)
            print('*'*80, file=sys.stderr)
            print('', file=sys.stderr)
            time.sleep(3)
            sys.exit(1)

    return groups





def generate_func(group, item, t):
    result_scalar = """
integer function {0}(trex_file, value) result(info)
    implicit none
    integer*8, intent(in) :: trex_file
    {1}, intent(in)       :: value
    call {2}(value)
    info = 0
end function {0}
"""

    result_array = """
integer function {0}(trex_file,value) result(info)
    implicit none
    integer*8, intent(in) :: trex_file
    {1}, intent(in) :: value{2}
    call {3}(value)
    info = 0
end function {0}
"""

    func_name = "trexio_{0}_{1}_{2}".format(t, group, item[0])
    ezfio_name = func_name.replace("trexio","ezfio")
    typ = item[1]
    dim = "(" + ",".join([":" for i in item[2]]) + ")"

    if dim == "()":
        return result_scalar.format(func_name, typ, ezfio_name)
    else:
        return result_array.format(func_name, typ, dim, ezfio_name)



def generate_interface(group, item, t):
    result_scalar = """
  interface
    integer function {0}(trex_file,value)
      integer*8, intent(in) :: trex_file
      {1}, intent(in) :: value
    end function {0}
  end interface
"""

    result_array = """
  interface
    integer function {0}(trex_file,value)
      integer*8, intent(in) :: trex_file
      {1}, intent(in) :: value{2}
    end function {0}
  end interface
"""

    func_name = "trexio_{0}_{1}_{2}".format(t, group, item[0])
    typ = item[1]
    dim = "(" + ",".join([":" for i in item[2]]) + ")"

    if dim == "()":
        return result_scalar.format(func_name, typ)
    else:
        return result_array.format(func_name, typ, dim)



def generate_set_func(group, item):
    return generate_func(group, item, "set")

def generate_get_func(group, item):
    return generate_func(group, item, "get")


def generate_set_interface(group, item):
    return generate_interface(group, item, "set")

def generate_get_interface(group, item):
    return generate_interface(group, item, "get")






def generate_module(config):
    text = [ """
module trexio
  integer, parameter  :: TREXIO_SUCCESS = 0

  interface
    integer function trexio_open(filename, mode, trex_file)
      character*(*), intent(in) :: filename
      character*(*), intent(in) :: mode
      integer*8, intent(out)    :: trex_file
    end function trexio_open
  end interface

  interface
    integer function trexio_close(trex_file)
      integer*8, intent(in) :: trex_file
    end function trexio_close
  end interface
"""
    ]
    for group in config:
        for item in config[group]:
            text += [
                generate_set_interface(group, item),
                generate_get_interface(group, item)
            ]
    text += [ "end module trexio" ]

    with open('trexio.f90','w') as f:
        f.write("\n".join(text))



def generate_functions(config):
    text = [ """
integer function trexio_open(filename, mode, trex_file) result(info)
  implicit none
  character*(*), intent(in) :: filename
  character*(*), intent(in) :: mode
  integer*8, intent(out)    :: trex_file
  call ezfio_set_file(filename)
  trex_file = 1234567_8
  info = 0
end function trexio_open

integer function trexio_close(trex_file) result(info)
  implicit none
  integer*8, intent(in) :: trex_file
  call ezfio_finish()
  info = 0
end function trexio_close

"""
    ]
    for group in config:
        for item in config[group]:
            text += [
                generate_set_func(group, item),
                generate_get_func(group, item)
            ]

    with open('trexio_functions.f90','w') as f:
        f.write("\n".join(text))


def main():
    config = read_config(sys.argv[1])
    generate_module(config)
    generate_functions(config)



if __name__ == "__main__":
    main()
