-- xmtn-hooks.lua --- mtn Lua hook functions used in all xmtn automate
-- stdio sessions
--
-- Copyright (C) 2010 Stephen Leake
--
-- Author: Stephen Leake
-- Keywords: tools
--
-- This file is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This file is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this file; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
-- Boston, MA  02110-1301  USA.

function get_netsync_connect_command(uri, args)

    local argv = nil

    if uri["scheme"] == "ssh" then
        argv = { "ssh" }

        if uri["user"] then
            table.insert(argv, "-l")
            table.insert(argv, uri["user"])
        end
        if uri["port"] then
            table.insert(argv, "-p")
            table.insert(argv, uri["port"])
        end

        table.insert(argv, uri["host"])

        if xmtn_sync_ssh_exec then
            if xmtn_sync_ssh_exec [uri["host"]] then
               table.insert(argv, xmtn_sync_ssh_exec [uri["host"]])
            else
               table.insert(argv, "mtn")
            end
        else
            table.insert(argv, "mtn")
        end
        
        if args["debug"] then
            table.insert(argv, "--verbose")
        else
            table.insert(argv, "--quiet")
        end

        table.insert(argv, "--db")
        table.insert(argv, uri["path"])
        table.insert(argv, "serve")
        table.insert(argv, "--stdio")
        table.insert(argv, "--no-transport-auth")


    elseif uri["scheme"] == "file" then
        if xmtn_sync_file_exec then
            argv = { xmtn_sync_file_exec }
        else
            if string.sub(get_ostype(), 1, 6) == "CYGWIN" then
                -- assume Cygwin mtn is not first in path
                argv = { "c:/bin/mtn" }
            else
                -- otherwise assume first mtn in path is correct
                argv = { "mtn" }
            end
        end

        if args["debug"] then
            table.insert(argv, "--verbose")
        else
            table.insert(argv, "--quiet")
        end

        table.insert(argv, "--db")
        table.insert(argv, uri["path"])
        table.insert(argv, "serve")
        table.insert(argv, "--stdio")
        table.insert(argv, "--no-transport-auth")

    elseif uri["scheme"] == "mtn" then
        argv = {}

    else
        error(uri["scheme"] .. " not supported")
    end
    return argv
end

-- end of file
