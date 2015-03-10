(ert-deftest test-org-cite/html-to-org
    (should (string=
	     (org-cite--html-to-org "<b>foo</b>")
	     "*foo*"))
  (should (string=
	   (org-cite--html-to-org "<i>foo</i>")
	   "/foo/"))
  (should (string=
	   (org-cite--html-to-org "<sub>foo</sub>")
	   "_{foo}"))
  (should (string=
	   (org-cite--html-to-org "<sup>foo</sup>")
	   "^{foo}"))
  (should (string=
	   (org-cite--html-to-org "<span style=\"text-decoration:underline;\">foo</span>")
	   "_foo_")))
