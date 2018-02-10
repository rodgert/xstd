/*
    Copyright (c) 2018 Thomas Rodgers <rodgert@twrodgers.com>

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/
#ifndef XASIO_READ_HPP
#define XASIO_READ_HPP

#include "../include/expected.hpp"

#include <boost/asio/read.hpp>

namespace xasio {
    using read_result_type = xstd::expected<size_t, boost::system::error_code>;

    template<typename SyncReadStream,
             typename MutableBufferSequence>
    read_result_type read(SyncReadStream & s, MutableBufferSequence const & buffers) {
        boost::system::error_code ec;
        auto res = boost::asio::read(s, buffers, ec);
        if (ec)
            return ec;
        return res;
    }

    template<typename AsyncReadStream,
             typename MutableBufferSequence,
             typename ReadHandler>
    void async_read(AsyncReadStream & s, MutableBufferSequence const& buffers,
                        ReadHandler handler) {
        boost::asio::async_read(s, buffers,
                [handler](boost::system::error_code& ec, size_t bytes_transferred) {
                    if (ec) {
                        handler(xstd::make_unexpected(ec));
                    } else {
                        handler(xstd::make_expected(bytes_transferred));
                    }
                });
    }
} // namespace xasio
#endif // XASIO_READ_HPP
