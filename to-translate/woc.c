/*
 * woc - word occurance
 *
 * usage: woc [words...] -- [files or directories...]
 *
 * Count the occurances of words... in files... printing the score to
 * standard out. If a word starts with '-' then reduce the count by 1
 * for each occurance of the given word.
 *
 * If a directory is specified after the -- then all the files in the
 * directory are scanned.
 *
 * Author: Burton Samograd <burton.samograd@gmail.com> 2012
 * License: AGPL
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/syslimits.h>

int count_occurances(char** words, int num_words, char* filename) {
  FILE* file = fopen(filename, "r");
  if(!file) {
    fprintf(stderr, "fopen: %s: ", filename); perror("");
    exit(1);
  }
  fseek(file, 0, SEEK_END);
  int size = ftell(file);

  char* str = mmap(NULL, size, PROT_READ, MAP_PRIVATE, fileno(file), 0);
  if(!str) {
    perror("mmap: ");
    exit(1);
  }

  int cur_word, i, match_count = 0;
  for(cur_word = 0; cur_word < num_words; cur_word++) {
    char* word = words[cur_word];
    int inc;
    if(*word == '-') {
      word++;
      inc = -1;
    } else {
      inc = 1;
    }
    int wordlen = strlen(word);
    
    for(i = 0; i < size; i++) {
      if(toupper(str[i]) == *word) {
	int j;
	for(j = 1;  word[j] == toupper(str[i+j]) && j < wordlen; j++);
	if(j == wordlen) {
	  match_count += inc;
	}
      }
    }
  }

  munmap(str, size);
  fclose(file);

  return match_count;
}

void woc(char* file, char** words, int num_words) {
    int match_count;
    /* see if a directory is specified */
    DIR* dir = opendir(file);
    if(dir) {
      /* we got a directory */
      int filesilen = strlen(file);
      if(file[filesilen-1] == '/') {
	/* strip off trailing / if present */
	file[filesilen-1] = '\0';
      }
      struct dirent* entry;
      char path[PATH_MAX];
      while((entry = readdir(dir))) {
	/* traverse the files in the directory */
	struct stat buf;
	if(!(!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, ".."))) {
            snprintf(path, PATH_MAX, "%s/%s", file, entry->d_name);
	    stat(path, &buf);
	    if(S_ISDIR(buf.st_mode)) {
	        /* Recurse subdirectories */
	        woc(path, words, num_words);
            } else {
    	        /* do match count */
	        match_count = count_occurances(words, num_words, path);
	        if(match_count > 0) {
	            fprintf(stdout, "%d\t%s\n", match_count, path);
	        }
            }
         }
      }
      closedir(dir);
    } else {
      /* we have a file specified, do match count */
      match_count = count_occurances(words, num_words, file);
      if(match_count > 0) {
	fprintf(stdout, "%d\t%s\n", match_count, file);
      }
    }
}

void usage(void) {
  fprintf(stderr, "usage: woc [words...] -- [files or directories...]\n");
  fprintf(stderr, "count the number of occurances of words in files or files in directories\n");
}

int main(int argc, char **argv) {
  int help_requested = 0, i, j;
  int words_start = 1, num_words = 0;
  int files_start = -1, num_files = 0;
  for(i = 1; i < argc; i++) {
    if(!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
      help_requested = 1;
      break;
    } else if(!strcmp(argv[i], "--")) {
      files_start = i+1;
      continue;
    }

    if(files_start > 0) {
      num_files++;
    } else {
      num_words++;
    }
  }

  if(argc < 2 || help_requested) {
    usage();
    exit(1);
  }

  char use_defaults = num_files == 0;
  if(num_files == 0) {
      num_files++;
  }
  char* words[num_words];
  char* files[num_files];
  if(use_defaults) {
      files[0] = ".";
  }
  for(i = words_start; i < words_start + num_words; i++) {
    /* uppercase word */
    for(j = 0; j < strlen(argv[i]); j++) {
      argv[i][j] = toupper(argv[i][j]);
    }
    words[i-words_start] = argv[i];
  }
  if(!use_defaults) {
      for(i = files_start; i < files_start + num_files; i++) {
        files[i-files_start] = argv[i];
      }
  }
  for(i = 0; i < num_files; i++) {
      woc(files[i], words, num_words);
  }
return 0;
}
