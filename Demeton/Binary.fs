module Demeton.Binary

open System.IO

/// <summary>Writes the specified byte value to a stream.</summary>
/// <param name="value">The byte value to be written.</param>
/// <param name="stream">The stream the byte value should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeByte (value: byte) (stream: Stream): Stream =
    stream.WriteByte(value)
    stream


let readByte (stream: Stream) =
    let read = stream.ReadByte()
    match read with
    | -1 -> invalidOp "Unexpected EOF reached in the stream."
    | _ -> (byte)read


/// <summary>Writes the specified byte array to a stream.</summary>
/// <param name="value">The byte array to be written.</param>
/// <param name="stream">The stream the byte array should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeBytes (bytes: byte[]) (stream: Stream): Stream =
    stream.Write (bytes, 0, bytes.Length)
    stream


// todo: make the buffer size configurable
let readBytes length (stream: Stream): byte[] =
    // todo: this method is reusable
    let rec copyToStream 
        bytesToCopy
        (buffer: byte[]) 
        (fromStream: Stream) 
        (toStream: Stream): unit =

        let maxBytesToRead = min bytesToCopy buffer.Length
        let bytesRead = fromStream.Read(buffer, 0, maxBytesToRead)
        toStream.Write(buffer, 0, bytesRead)

        let remainingLength = bytesToCopy - bytesRead
        match remainingLength with
        | 0 -> ignore()
        | _ -> copyToStream remainingLength buffer fromStream toStream

    use bufferStream = new MemoryStream()

    let bufferSize = min length (1024 * 1024)
    let buffer: byte[] = Array.zeroCreate bufferSize

    copyToStream length buffer stream bufferStream
    bufferStream.ToArray()


/// <summary>
/// Writes the specified integer value to a stream using the big endian order.
//// </summary>
/// <param name="value">The integer value to be written.</param>
/// <param name="stream">The stream the integer value should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianInt32 (value: int) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)


let readBigEndianInt32 (stream: Stream): int =
    (((int)(readByte stream)) <<< 24)
    ||| (((int)(readByte stream)) <<< 16)
    ||| (((int)(readByte stream)) <<< 8)
    ||| (((int)(readByte stream)))


/// <summary>
/// Writes the specified unsigned integer value to a stream using the big 
/// endian order.
//// </summary>
/// <param name="value">The unsigned integer value to be written.</param>
/// <param name="stream">
/// The stream the unsigned integer value should be written to.
/// </param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianUInt32 (value: uint32) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)


let readBigEndianUInt32 (stream: Stream): uint32 =
    (((uint32)(readByte stream)) <<< 24)
    ||| (((uint32)(readByte stream)) <<< 16)
    ||| (((uint32)(readByte stream)) <<< 8)
    ||| (((uint32)(readByte stream)))

