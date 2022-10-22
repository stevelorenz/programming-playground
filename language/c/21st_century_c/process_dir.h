struct filestruct;
typedef void (*level_fn)(struct filestruct path);

typedef struct filestruct {
	char *name;
	char *fullname;
	level_fn directory_action;
	level_fn file_action;
	int depth;
	int error;
	void *data;
} filestruct;

#define process_dir(...) process_dir_r((filestruct){__VA_ARGS__})

int process_dir_r(filestruct level);
