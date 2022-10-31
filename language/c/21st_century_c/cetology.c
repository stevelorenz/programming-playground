/*
 * cetology.c
 * Copyright (C) 2022 zuoxiang <zuoxiang@ZUOXIANG-M-5G23>
 *
 * Distributed under terms of the MIT license.
 */

#include <stdio.h>

#include "fstr.h"

int main() {
	fstr_s *fstr = fstr_new("moby");

	fstr_list chapters = fstr_split(fstr, "\nCHAPTER");
	int i = 0;
	for (i = 0; i < chapters.count; ++i) {
		fstr_list for_the_title = fstr_split(chapters.strings[i], "\\.");
		fstr_show(for_the_title.strings[1]);
		fstr_list me = fstr_split(chapters.strings[i], "\\WI\\W");
		fstr_list whales = fstr_split(chapters.strings[i], "whale(s|)");
		fstr_list words = fstr_split(chapters.strings[i], "\\W");
		printf("\nch %i, words: %i.\t Is: %i\twhales: %i\n", i, words.count - 1,
			   me.count - 1, whales.count - 1);
		fstr_list_free(for_the_title);
		fstr_list_free(me);
		fstr_list_free(whales);
		fstr_list_free(words);
	}
	fstr_list_free(chapters);
	fstr_free(fstr);

	return EXIT_SUCCESS;
}
