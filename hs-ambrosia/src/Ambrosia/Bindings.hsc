#include <bindings.dsl.h>
#include <ambrosia/client.h>

module Ambrosia.Bindings where

import Foreign
import Foreign.C.String
import Foreign.C.Types

#num AMBROSIA_HEADERSIZE

#starttype struct log_hdr
#field commitID , Int32
#field totalSize , Int32
#field checksum , Int64
#field seqID , Int64
#stoptype

#integral_t enum MsgType
#num RPC
#num AttachTo
#num TakeCheckpoint
#num RPCBatch
#num Checkpoint
#num InitialMessage
#num UpgradeTakeCheckpoint
#num TakeBecomingPrimaryCheckpoint
#num UpgradeService

#num AMBCLIENT_DEFAULT_BUFSIZE

#ccall amb_sleep_seconds , CDouble -> IO ()
#ccall startup_protocol , CInt -> CInt -> IO ()
#ccall connect_sockets , Ptr CInt -> Ptr Int -> IO ()
#ccall amb_write_incoming_rpc , Ptr () -> Int32 -> CChar -> Ptr () -> CInt -> IO (Ptr ())
#ccall amb_write_outgoing_rpc_hdr , Ptr () -> CString -> Int32 -> CChar -> Int32 -> CChar -> CInt -> IO (Ptr ())
#ccall amb_send_outgoing_rpc , Ptr () -> CString -> Int32 -> CChar -> Int32 -> CChar -> Ptr () -> CInt -> IO ()
#ccall amb_recv_log_hdr , CInt -> Ptr <struct log_hdr> -> IO ()
#ccall write_zigzag_int , Ptr () -> Int32 -> IO (Ptr ())
#ccall read_zigzag_int , Ptr () -> Ptr Int32 -> IO (Ptr ())
#ccall zigzag_int_size , Int32 -> CInt
#ccall amb_get_error_string , IO CString
