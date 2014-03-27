PostgreSQL LISTEN Example
=========================
A simple OTP application that uses `epgsql` to act as a listener for PostgreSQL `NOTIFY` messages.

Running the Application
-----------------------
The application assumes that you have a PostgreSQL version that allows notification payloads (9.0+).

Change the hard-coded PostgreSQL credentials in `src/erlln_srv.erl` (Lines 7 through 10), and run `make`:

    $ make
    ./rebar get-deps
    WARN:  Expected erlln/deps/epgsql to be an app dir (containing ebin/*.app), but no .app found.
    ==> erlln (get-deps)
    WARN:  Expected erlln/deps/epgsql to be an app dir (containing ebin/*.app), but no .app found.
    Pulling epgsql from {git,"https://github.com/wg/epgsql.git",{tag,"1.4"}}
    Cloning into 'epgsql'...
    ==> epgsql (get-deps)
    ./rebar compile
    ==> epgsql (compile)
    Compiled src/pgsql.erl
    Compiled src/pgsql_binary.erl
    Compiled src/pgsql_connection.erl
    Compiled src/pgsql_idatetime.erl
    Compiled src/pgsql_fdatetime.erl
    Compiled src/pgsql_sock.erl
    Compiled src/pgsql_types.erl
    ==> erlln (compile)
    Compiled src/erlln_app.erl
    Compiled src/erlln_sup.erl
    Compiled src/erlln_srv.erl

Once compiled, run the application:

    $ make run
    erlln server started

Now in a psql session in another terminal, send your notifications:

    postgres=# NOTIFY erlln, 'Test payload';
    NOTIFY

And you should see the following in the terminal running the erlln application:

    Received NOTIFY on channel erlln: Test payload

The application is listening for a payload of `stop` to shutdown. In the psql session:

    postgres=# NOTIFY erlln, 'stop';
    NOTIFY

And back in the terminal running the erlln application you should see:

    Received stop request via NOTIFY
    Shutting down
    Terminating: normal

Most of the code central to the LISTEN behavior is in `src/erlln_srv.erl` and the rest is just setting up the OTP application.
