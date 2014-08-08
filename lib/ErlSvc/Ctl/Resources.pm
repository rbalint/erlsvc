#-
# Copyright 2011 Yakaz. All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
#    2. Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY YAKAZ ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL YAKAZ OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
# OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package ErlSvc::Ctl::Resources;

use strict;
use warnings;
use utf8;

use User::pwent;
use User::grent;

sub opts ($) {
    my ($self, $opt) = @_;

    my $val = $self->cache->get('global_opts')->{$opt};

    unless (defined $val) {
        my $yaml = $self->cache->get('config');
        $val = $yaml->[0]->{$opt} if (defined $yaml);
    }

    unless (defined $val) {
        $val = $self->cache->get('default_config')->{$opt};
    }

    return $val;
}

sub opts_to_list (;$) {
    my ($self, $opts) = @_;

    $opts = $self->cache->get('global_opts') unless ($opts);

    my @list = ();

    foreach my $opt (keys %$opts) {
        my $val = $opts->{$opt};

        my $opt_flag = '--'.$opt;
        $opt_flag =~ s/_/-/go;

        if (ref($val) eq 'ARRAY') {
            foreach my $v (@$val) {
                my $val_escaped = $v;
                $val_escaped =~ s/(["' ])/\\$1/g;
                push @list, $opt_flag, $val_escaped;
            }
        } elsif (ref($val) eq 'HASH') {
            while (my ($key, $v) = each %$val) {
                my $val_escaped = $v;
                $val_escaped =~ s/(["' ])/\\$1/g;
                push @list, $opt_flag, "$key=$val_escaped";
            }
        } else {
            if ($val eq '0' || $val eq '1') {
                push @list, $opt_flag;
            } else {
                my $val_escaped = $val;
                $val_escaped =~ s/(["' ])/\\$1/g;
                push @list, $opt_flag, $val_escaped;
            }
        }
    }

    return @list;
}

sub node (;%) {
    my ($self, %options) = @_;

    $self->_initialize_nodes() unless ($options{'no_init'});

    $self->cache->get('target_node');
}

sub controller (;%) {
    my ($self, %options) = @_;

    $self->_initialize_nodes() unless ($options{'no_init'});

    $self->cache->get('controller_node');
}

sub _initialize_nodes () {
    my ($self) = @_;

    return 1 if ($self->cache->get('controller_node'));

    # The controller node is an Erlang node on this local host. It's used
    # as the source for every Erlang calls.
    my $controller = ErlSvc::Ctl::Erlang::Node->new($self,
        "erlsvc-controller-$$");
    $controller->flag_as_controller;
    $controller->set_releases_dir($self->opts('releases_dir'));
    $self->cache->set('controller_node', $controller);
    $self->log->debug("APP", "Working from controller node '$controller'\n");

    # The target node is an Erlang node on this local host or on a
    # remote host. This is where the commands will mostly run.
    my $target = ErlSvc::Ctl::Erlang::Node->new_from_app_opts($self);

    # If the user specified extra_flags and erllibs_path, setup the
    # target node for these.
    my $erlapp_args = $self->opts('erlapp_args');
    if ($erlapp_args && ref($erlapp_args) eq 'HASH') {
        $target->set_erl_app_args(%$erlapp_args);
    }
    my $extra_flags = $self->opts('extra_flags');
    if ($extra_flags && ref($extra_flags) eq 'ARRAY') {
        $target->add_erl_cmd_args(@$extra_flags);
    }
    my $erllibs_path = $self->opts('erllibs_path');
    if ($erllibs_path && ref($erllibs_path) eq 'ARRAY') {
        $target->proc->set_env('ERL_LIBS' => join(':', @$erllibs_path));
    }

    $self->cache->set('target_node', $target);
    $self->log->debug("APP", "Working with target node '$target'\n");

    # The controller node and the target node share the same cookie.
    $controller->set_cookie($target->cookie);

    $self->erl_env->finish_init();

    $self->log->debug("APP", "<b>Nodes initialized</b>\n");

    return 1;
}

sub user () {
    shift->opts('user');
}

sub user_changed (;$) {
    my ($self, $user) = @_;

    my $uid = $self->uid($user);
    return 0 unless (defined $uid);

    return $uid != $<;
}

sub uid (;$) {
    my ($self, $user) = @_;

    $user = $self->user unless ($user);
    return unless $user;

    my $pwent = getpwnam($user);
    return unless $pwent;

    return $pwent->uid;
}

sub home (;$) {
    my ($self, $user) = @_;

    $user = $self->user unless ($user);
    return unless $user;

    my $pwent = getpwnam($user);
    return unless $pwent;

    return $pwent->dir;
}

sub group () {
    shift->opts('group');
}

sub group_changed (;$) {
    my ($self, $group) = @_;

    my $gid = $self->gid($group);
    return 0 unless (defined $gid);

    return $gid != $(;
}

sub gid (;$) {
    my ($self, $group) = @_;

    $group = $self->group unless ($group);
    return unless $group;

    my $grent = getgrnam($group);
    return unless $grent;

    return $grent->gid;
}

sub erl_env (;%) {
    my ($self, %options) = @_;

    $self->_initialize_nodes() unless ($options{'no_init'});

    $self->cache->get('erl_env');
}

sub log () {
    shift->cache->get('logger');
}

sub verbose (;$) {
    my ($self, $component) = @_;

    my $verbose = $self->opts('verbose');
    return unless (defined $verbose);

    my @components = @$verbose;
    return 0 if (scalar @components == 0);

    if (scalar @components == 1) {
        return 0 if $components[0] eq '!ALL';
        return 1 if $components[0] eq 'ALL';
        return 1 if $components[0] eq '';
    }

    if ($component) {
        if (grep(/^!$component$/, @components)) {
            return 0;
        }
        unless (grep(/^$component$/, @components) ||
            grep(/^ALL$/, @components)) {
            return 0;
        }
    }

    return 1;
}

1;
