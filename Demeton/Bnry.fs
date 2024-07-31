/// <summary>
/// Contains functions for reading and writing of binary data in streams.
/// </summary>
[<RequireQualifiedAccess>]
module Bnry

open System.IO

/// <summary>Writes the specified byte value to a stream.</summary>
/// <param name="value">The byte value to be written.</param>
/// <param name="stream">The stream the byte value should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeByte (value: byte) (stream: Stream) : Stream =
    stream.WriteByte(value)
    stream

/// <summary>
/// Reads a byte from the stream. If the stream has reached its end, throws
/// and exception.
/// </summary>
let readByte (stream: Stream) =
    let read = stream.ReadByte()

    match read with
    | -1 -> invalidOp "Unexpected EOF reached in the stream."
    | _ -> byte read


/// <summary>Writes the specified byte array to a stream.</summary>
/// <param name="bytes">The byte array to be written.</param>
/// <param name="stream">The stream the byte array should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeBytes (bytes: byte[]) (stream: Stream) : Stream =
    stream.Write(bytes, 0, bytes.Length)
    stream

/// <summary>
/// Copies the specified number of bytes from one stream to another.
/// </summary>
let rec copyToStream
    length
    (buffer: byte[])
    (fromStream: Stream)
    (toStream: Stream)
    : unit =

    let maxBytesToRead = min length buffer.Length
    let bytesRead = fromStream.Read(buffer, 0, maxBytesToRead)
    toStream.Write(buffer, 0, bytesRead)

    let remainingLength = length - bytesRead

    match remainingLength with
    | 0 -> ignore ()
    | _ -> copyToStream remainingLength buffer fromStream toStream

/// <summary>
/// Reads the specified number of bytes from the stream and returns the
/// resulting byte array.
/// </summary>
let readBytes bufferLength length (stream: Stream) : byte[] =
    use bufferStream = new MemoryStream()

    let bufferSize = min length bufferLength
    let buffer: byte[] = Array.zeroCreate bufferSize

    copyToStream length buffer stream bufferStream
    bufferStream.ToArray()

/// <summary>
/// Writes the specified 32-bit signed integer value to a stream using the
/// big endian byte order.
//// </summary>
/// <param name="value">The integer value to be written.</param>
/// <param name="stream">The stream the integer value should be written to.
/// </param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianInt32 (value: int) (stream: Stream) : Stream =
    stream
    |> writeByte ((byte) (value >>> 24))
    |> writeByte ((byte) (value >>> 16))
    |> writeByte ((byte) (value >>> 8))
    |> writeByte ((byte) value)

/// <summary>
/// Reads a big-endian encoded 32-bit signed integer from the stream.
/// </summary>
let readBigEndianInt32 (stream: Stream) : int =
    (((int) (readByte stream)) <<< 24)
    ||| (((int) (readByte stream)) <<< 16)
    ||| (((int) (readByte stream)) <<< 8)
    ||| (((int) (readByte stream)))

/// <summary>
/// Writes the specified 32-bit unsigned integer value to a stream using the big
/// endian order.
//// </summary>
/// <param name="value">The 32-bit unsigned integer value to be written.</param>
/// <param name="stream">
/// The stream the 32-bit unsigned integer value should be written to.
/// </param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianUInt32 (value: uint32) (stream: Stream) : Stream =
    stream
    |> writeByte ((byte) (value >>> 24))
    |> writeByte ((byte) (value >>> 16))
    |> writeByte ((byte) (value >>> 8))
    |> writeByte ((byte) value)

/// <summary>
/// Reads a big-endian encoded 32-bit unsigned integer from the stream.
/// </summary>
let readBigEndianUInt32 (stream: Stream) : uint32 =
    (((uint32) (readByte stream)) <<< 24)
    ||| (((uint32) (readByte stream)) <<< 16)
    ||| (((uint32) (readByte stream)) <<< 8)
    ||| (((uint32) (readByte stream)))
