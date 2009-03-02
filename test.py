#!/usr/bin/env python

import memcache
import optparse
import time

def generate_test_payload(size):
    return "x" * size


def main():
    parser = optparse.OptionParser()
    parser.add_option("--host", help="specify the host (default=127.0.0.1)", metavar="HOST",
                      default="127.0.0.1")
    parser.add_option("-p", "--port", help="specify the port (default=2345)", metavar="PORT", 
                      default="2345")
    parser.add_option("-s", "--payload-size", help="specify the payload size (default=512)",
                      metavar="SIZE", default="512")
    parser.add_option("-n", "--num-messages", help="specify the number of messages (default=1000)",
                      metavar="NUM", default="1000")
    options, args = parser.parse_args()

    mc = memcache.Client([':'.join([options.host, options.port])], debug=0)

    options.payload_size = int(options.payload_size)
    options.num_messages = int(options.num_messages)
    
    test_payload = generate_test_payload(options.payload_size)

    errors = 0
    start = time.time()
    for i in range(options.num_messages):
        if not mc.set("foo", test_payload):
            errors += 1
    end = time.time()
    diff = end - start
    print "Writing %d messages of size %d took %0.3f seconds - %0.3f m/s (with %d errors)" % (options.num_messages, options.payload_size, diff, (options.num_messages/diff), errors)

    responses = []
    start = time.time()
    for i in range(options.num_messages):
        responses.append(mc.get("foo"))
    end = time.time()
    diff = end - start
    print "Reading %d messages took %0.3f seconds - %0.3f m/s" % (options.num_messages, diff, (options.num_messages/diff))

    print "Checking response correctness."

    errors = 0
    for response in responses:
        if response != test_payload:
            errors += 1

    print "Found %d errors in %d responses." % (errors, len(responses))


if __name__ == "__main__":
    main()

