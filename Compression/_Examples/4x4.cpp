// (c) Bulat Ziganshin <Bulat.Ziganshin@gmail.com>
// Multithreaded compression with overlapped I/O
// 4x4 version 0.3 - now in C++

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"
#include "../MultiThreading.h"
#define Wait   Lock


// *************************************************************************************************
// *** Global variables ****************************************************************************
// *************************************************************************************************

// Compression job
struct Job
{
    Event  Finished;        // Signals that job was finished
    Event  Compressed;      // Signals that compression part of job was finished
    Event  Saved;           // Signals that writing part of job was finished
    int    insize;          // Amount of data in inbuf
    int    outsize;         // Amount of data in outbuf
    void  *outbuf;          // Buffer pointing to output (compressed) data
};

bool ENCODE;                              // True for compression, False - decompression
char *method = "tor";                     // Compression method used for every block
int blocksize = 8*mb;                     // Size of chunks input split to
int NumIOThreads = 4;                     // Max. number of threads doing I/O
int NumCompressionThreads = 4;            // Max. number of threads doing compression
SyncQueue<Job*> WriterJobs;               // Queue of jobs for Writer thread, ordered by input chunks
Semaphore AllowCompression;               // Limits number of threads doing compression
Semaphore IO;                             // Prevents simultaneous execution of I/O operations from different threads
FILE *infile, *outfile;                   // Input/output streams

void mywrite(void *buf, int size)
{
    if (fwrite(buf, 1, size, outfile) != size)
        throw "mywrite:fwrite";
}

void write4(int value)
{
    unsigned char buf[4];
    buf[0] = ((unsigned)value);
    buf[1] = ((unsigned)value)>>8;
    buf[2] = ((unsigned)value)>>16;
    buf[3] = ((unsigned)value)>>24;
    mywrite(buf, 4);
}

int read4()
{
    unsigned char buf[4];
    if (fread (buf, 1, 4, infile) != 4) {
        throw "read4:fread";
    }
    return (((((buf[3]<<8)+buf[2])<<8)+buf[1])<<8)+buf[0];
}

// Read ASCIIZ string from file
void fgets0(char *buf, int size, FILE *infile)
{
    while (--size)
        if ( (*buf++ = fgetc(infile)) == '\0')
            return;
    *buf = '\0';
}


// *************************************************************************************************
// *** (De)compression machine *********************************************************************
// *************************************************************************************************

int callback_func (const char *what, void *data, int size, void *param)
{
    Job *job = (Job*) param;

    if (strequ(what,"read") && ENCODE)
    {
        IO.Lock();              // Lock guarantees that jobs will be queued to WriterJobs in the same order as blocks are read from input stream
        job->insize = fread(data, 1, mymin(size,blocksize), infile);
        if (job->insize==0)   job->insize = -1;
        WriterJobs.Put(job);     // Put job to the Writer ordering queue
        IO.Release();
        if (job->insize != -1)
            AllowCompression.Lock();     // wait for compression slot
        return job->insize;
    }
    else if (strequ(what,"read") && !ENCODE)
    {
        if (job->insize==0)  return 0;
        int len = fread(data, 1, mymin(size,job->insize), infile);
        job->insize -= len;
        if (job->insize == 0)
        {
            IO.Release();
            AllowCompression.Lock();    // wait for decompression slot
        }
        return len;
    }
    else if (strequ(what,"write"))
    {
        AllowCompression.Release();   // release (de)compression slot

        job->outbuf  = data;
        job->outsize = size;
        job->Compressed.Signal();    // send signal to Writer thread

        job->Saved.Wait();           // pause execution until (de)compressed data will be written
        return size;
    }
    return 0;
}


// Read and (de)compress data by chunks from input stream
static DWORD WINAPI DeCompressionThread (void *paramPtr)
{
    Job *job = (Job*) paramPtr;

    try
    {
        for(;;)
        {
            if (ENCODE)
            {
                if (Compress (method, callback_func, job) < 0)  break;
            }
            else
            {
                IO.Lock();
                if (ftell(infile)==filelength(fileno(infile)))
                    {job->insize = -1; WriterJobs.Put(job); IO.Release(); break;}
                job->outsize = read4();
                job->insize = read4();
                char method[1000];
                fgets0(method, sizeof(method), infile);
                job->insize -= strlen(method)+1;
                WriterJobs.Put(job);     // Put job to the Writer ordering queue
                Decompress(method, callback_func, job);
            }
        }
    }
    catch (char *msg)
    {
        fprintf(stderr,"\n%s\n", msg);
    }
    job->Finished.Signal();
    return 0;
}


// Send compressed data to output stream
void WriterThread(int NumThreads)
{
    try
    {
        for(;;)
        {
            // Acquire next writer job
            Job *job = WriterJobs.Get();

            // Break on EOF/error
            if (job->insize < 0)
            {
                job->Finished.Wait();
                NumThreads--;
                if (NumThreads <= 0)
                    break;
                continue;
            }


            // Wait until (de)compression will be finished
            job->Compressed.Wait();

            // Prevent simultaneous read and write (?)
            IO.Lock();
            if (ENCODE)
            {
                // Save compressed block header: original/compressed blocksize and compression method
                write4(job->insize);
                write4(job->outsize + strlen(method)+1);
                mywrite(method, strlen(method)+1);
            }
            // Save (de)compressed data
            mywrite(job->outbuf, job->outsize);
            IO.Release();

            // Signal that we've saved (de)compressed data
            job->Saved.Signal();
        }
    }
    catch (char *msg)
    {
        fprintf(stderr,"\n%s\n", msg);
    }
}


// Create (De)compression Machine and perform (de)compression
void DeCompressionMachine()
{
    // Number of threads doing I/O and compression
    int NumThreads = NumIOThreads + NumCompressionThreads;

    // Semaphore limiting amount of threads doing (de)compression
    AllowCompression.Create(NumCompressionThreads, NumCompressionThreads);

    // Semaphore limiting amount of threads doing I/O
    IO.Create(1,1);

    // Queue of jobs for Writer thread
    WriterJobs.SetSize(NumThreads);

    Job* job=new Job[NumThreads];

    // Create threads for I/O and compression
    for (int i=0; i < NumThreads; i++)
    {
        CThread t; t.Create(DeCompressionThread, &job[i]);
    }

    // Perform compression and write compressed data to outstream
    WriterThread(NumThreads);

    delete [] job;

    AllowCompression.Close();
    IO.Close();
    WriterJobs.Close();
}


int main(int argc, char **argv)
{
    try
    {
        if (argc==2)
        {
            infile  = stdin;   set_binary_mode(infile);
            outfile = stdout;  set_binary_mode(outfile);
        }
        else if (argc==4)
        {
            infile  = fopen (argv[2], "rb");   if (infile==NULL)  {printf ("Can't open input file %s!\n", argv[2]); return EXIT_FAILURE;}
            outfile = fopen (argv[3], "wb");   if (outfile==NULL) {printf ("Can't create output file %s!\n", argv[3]); return EXIT_FAILURE;}
        }
        else
        {
            printf("Usage: 4x4 method <infile >outfile\n"
                   "    or 4x4 method infile outfile\n");
            return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
        }

        compress_all_at_once = 1;
        method = argv[1];
        ENCODE = !strequ(method,"d");
        blocksize = GetDictionary(method);

        // Perform (de)compression
        DeCompressionMachine();

        return 0;
    }
    catch (char *msg)
    {
        fprintf(stderr,"\n%s\n", msg);
        return EXIT_FAILURE;
    }
}
