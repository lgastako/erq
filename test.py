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
    parser.add_option("-n", "--num-messages", help="specify the number of messages (default=1024)",
                      metavar="NUM", default="1024")
    parser.add_option("-q", "--queue_name", help="specify the name of the queue to use for testing",
                      metavar="QUEUE_NAME", default="hurley")
    parser.add_option("-r", "--read-only", help="perform read tests only.  assumes data is already present.", default=False, action="store_true")
    parser.add_option("-w", "--write-only", help="perform write tests only.", default=False, action="store_true")
    options, args = parser.parse_args()

    mc = memcache.Client([':'.join([options.host, options.port])], debug=0)

    options.payload_size = int(options.payload_size)
    options.num_messages = int(options.num_messages)

    test_payload = generate_test_payload(options.payload_size)

    total_data_in_mbs = (options.payload_size * options.num_messages) / 1024

    if options.read_only and options.write_only:
        raise Exception("Cannot enable read_only and write_only at the same time.")

    if not options.read_only:
        errors = 0
        start = time.time()
        for i in range(options.num_messages):
            if not mc.set(options.queue_name, test_payload):
                errors += 1
        end = time.time()
        diff = end - start
        print "Writing %d messages of size %d took %0.3f seconds - %0.3f msg/s - %0.3f MB/s (with %d errors)" % (options.num_messages, options.payload_size, diff, (options.num_messages/diff), (total_data_in_mbs/diff/1024), errors)

    if not options.write_only:
        responses = []
        start = time.time()
        for i in range(options.num_messages):
            responses.append(mc.get(options.queue_name))
        end = time.time()
        diff = end - start
        print "Reading %d messages took %0.3f seconds - %0.3f m/s - %0.3f MB/s" % (options.num_messages, diff, (options.num_messages/diff), (total_data_in_mbs/diff/1024))

    if not (options.read_only or options.write_only):
        print "Checking response correctness."

        errors = 0
        for response in responses:
            if response != test_payload:
                errors += 1

        print "Found %d errors in %d responses." % (errors, len(responses))


if __name__ == "__main__":
    main()

